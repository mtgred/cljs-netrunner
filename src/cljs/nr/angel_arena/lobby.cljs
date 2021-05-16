(ns nr.angel-arena
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :refer [capitalize lower-case]]
            [jinteki.cards :refer [all-cards]]
            [jinteki.utils :refer [superuser?]]
            [nr.ajax :refer [GET]]
            [nr.appstate :refer [app-state]]
            [nr.avatar :refer [avatar]]
            [nr.cardbrowser :refer [image-url]]
            [nr.deckbuilder :refer [deck-name]]
            [nr.deck-status :refer [deck-format-status-span]]
            [nr.game-row :refer [join-game]]
            [nr.player-view :refer [user-status-span]]
            [nr.sounds :refer [resume-sound]]
            [nr.translations :refer [tr tr-side tr-format]] ;XXX: Use tr-side and tr-format
            [nr.utils :refer [slug->format cond-button tristate-button faction-icon time-span-string]]
            [nr.ws :as ws]
            [reagent.core :as r]
            [reagent-modals.modals :as reagent-modals]))

(defonce arena-supported-formats [:standard :startup])

(defonce runs (r/atom nil))
(defonce chosen-format (r/atom (first arena-supported-formats)))
(defonce queueing (r/atom nil))
(defonce queue-times (r/atom nil))

(defn- fetch-runs []
  (go (let [{:keys [status json]} (<! (GET "/profile/angel-arena/runs"))]
        (when (= 200 status)
          (reset! runs (js->clj json))))))

(defn- fetch-queue-times []
  (go (let [{:keys [status json]} (<! (GET "/profile/angel-arena/queue-times"))]
        (when (= 200 status)
          (reset! queue-times (js->clj json))))))

; XXX: Copied from utils.clj.. maybe make cljc
(defn- get-wins
  [{:keys [games side] :as run-info}]
  (count (filter #(= (name side) (:winner %)) games)))

(defn- get-losses
  [{:keys [games] :as run-info}]
  (- (count (remove #(nil? (:winner %)) games))
     (get-wins run-info)))

(defn- deck-view [side s deck]
  (let [run-info (get-in @runs [@chosen-format side])
        wins (get-wins run-info)
        losses (get-losses run-info)
        time-since-start (- (js/Date.now) (js/Date.parse (:run-started run-info)))
        allowed-days (+ 3 wins losses)]
    [:div.deck
     [:img {:src (image-url (:identity deck))
            :alt (get-in deck [:identity :title] "")}]
     [:h4 (deck-name deck)]
     [:div.result.float-right (str wins " wins")]
     [:div (get-in deck [:identity :title])]
     [:div.result.float-right (str losses " losses")]
     [:div.time (str "Time left: " (time-span-string (/ (- (* 1000 60 60 24 allowed-days)
                                                           time-since-start)
                                                        1000)))]]))

  (defn- deck-buttons [side s deck]
    (r/with-let [abandon (r/atom false)]
      [:div.buttons
       [:div.button-row
        [tristate-button
         (tr [:angel-arena.queueing "Queueing..."])
         (tr [:angel-arena.queue-for-match "Queue for match"])
         (= (:_id deck) @queueing)
         (and @queueing
              (not= (:_id deck) @queueing))
         #(if @queueing
            (do (ws/ws-send! [:angel-arena/dequeue {:deck-id (:_id deck)}])
                (reset! queueing nil))
            (do (ws/ws-send! [:angel-arena/queue {:deck-id (:_id deck)}])
                (reset! queueing (:_id deck))))]
        "Average waiting time: " (time-span-string (get-in @queue-times [@chosen-format side]))]
       [:div.button-row
        (if @abandon
          [:span (tr [:angel-arena.are-you-sure "Are you sure?"]) " "
           [:button.small {:on-click #(do (ws/ws-send! [:angel-arena/abandon-run {:deck-id (:_id deck)}])
                                          (fetch-runs))} (tr [:angel-arena.are-you-sure-yes "yes"])]
           [:button.small {:on-click #(reset! abandon false)} (tr [:angel-arena.are-you-sure-no "no"])]]

          [cond-button (tr [:angel-arena.abandon-run "Abandon run"])
           (not @queueing)
           #(reset! abandon true)])]]))

  (defn- deck-games [side s deck]
    (let [run-info (get-in @runs [@chosen-format side])]
      [:div.games
       (doall
         (for [{:keys [game-id opponent winner] :as game} (:games run-info)]
           (let [result (cond
                          (nil? winner) "aborted"
                          (= winner (name side)) "won"
                          :else "lost")]
             [:div.match {:key game-id :class [result]}
              [:img.identity {:class [result]
                              :src (image-url (get @all-cards (:identity opponent)))
                              :alt (:identity opponent)
                              :title (str (:identity opponent) "\nOpponent: " (:username opponent))}]])))]))

  (defn- deckselect-modal [user {:keys [side decks]}]
    [:div
     [:h3 (tr [:angel-arena.select-deck "Select your deck"])]
     [:div.deck-collection.lobby-deck-selector
      (let [same-side? (fn [deck] (= (capitalize (name side))
                                     (get-in deck [:identity :side])))
            correct-format? (fn [deck] (let [form (get-in deck [:status :format])]
                                         (= (keyword form) @chosen-format)))
            legal? (fn [deck] (let [form (get-in deck [:status :format])]
                                (get-in deck [:status (keyword form) :legal])))]
        [:div
         (let [eligible-decks (->> @decks
                                   (filter same-side?)
                                   (filter correct-format?)
                                   (filter legal?)
                                   (sort-by :date >))]
           (if (empty? eligible-decks)
             [:div.infobox.one-line.blue-shade [:p (tr [:angel-arena.no-eligible-decks "No legal decks found for this side and format."])]]
             (doall
               (for [deck eligible-decks]
                 ^{:key (:_id deck)}
                 [:div.deckline {:on-click #(do (ws/ws-send! [:angel-arena/start-run
                                                              {:deck-id (:_id deck)}])
                                                (reagent-modals/close-modal!)
                                                (fetch-runs))}
                  [:img {:src (image-url (:identity deck))
                         :alt (get-in deck [:identity :title] "")}]
                  [:div.float-right [deck-format-status-span deck (get-in deck [:status :format]) true]]
                  [:h4 (:name deck)]
                  [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
                  [:p (get-in deck [:identity :title])]]))))])]])

  (defn- new-run-button-bar [side decks user]
    [:div.button-bar
     [cond-button (tr [:angel-arena.start-new-run "Start new run"])
      (not @queueing)
      #(reagent-modals/modal!
         [deckselect-modal user {:side side :decks decks}])]])

  (defmethod ws/-msg-handler :angel-arena/run-update [{{:keys [finished-run] :as data} :?data}]
    (when finished-run
      (println "Run finished :" data "\nWould display dialog box now..."))
    (fetch-runs))

(defn game-panel [decks s user]
  (r/create-class
    {:display-name "game-panel"

     :component-did-mount
     (fn []
       (fetch-runs)
       (fetch-queue-times))

     :reagent-render
     (fn []
       (if-not @runs
         [:div.game-panel.angel-arena
          [:h3 (tr [:angel-arena.requesting-run-data "Requesting run data..."])]]
         [:div.game-panel.angel-arena
          [:h3 (tr [:angel-arena.format "Format"])]
          [:div.format-bar
           (doall
             (for [form arena-supported-formats]
               ^{:key form}
               [:span.tab {:on-click #(reset! chosen-format form)
                           :class [(when (= @chosen-format form) "current")]}
                (get slug->format (name form))]))]
          [:h3 (tr [:angel-arena.active-corp-run "Active Corp run"])]
          (if (get-in @runs [@chosen-format :corp])
            (let [deck (first (filter #(= (str (:_id %))
                                          (get-in @runs [@chosen-format :corp :deck-id]))
                                      @decks))]
              [:div.run
               [deck-view :corp s deck]
               [deck-games :corp s deck]
               [deck-buttons :corp s deck]])
            [:div.run [new-run-button-bar :corp decks user]])

          [:h3 (tr [:angel-arena.active-runner-run "Active Runner run"])]
          (if (get-in @runs [@chosen-format :runner])
            (let [deck (first (filter #(= (str (:_id %))
                                          (get-in @runs [@chosen-format :runner :deck-id]))
                                      @decks))]
              [:div.run
               [deck-view :runner s deck]
               [deck-games :runner s deck]
               [deck-buttons :runner s deck]])
            [:div.run [new-run-button-bar :runner decks user]])

          [:h3 (tr [:angel-arena.latest-runs "Latest runs"])]]))}))

(defn- player-view
  ([player] (player-view player nil))
  ([player game]
   [:span.player
    [avatar (:user player) {:opts {:size 22}}]
    [user-status-span player]
    (let [side (:side player)
          faction (:faction (:identity (:deck player)))
          identity (:title (:identity (:deck player)))
          specs (:allow-spectator game)]
      (cond
        (and (some? faction)
             (not= "Neutral" faction)
             specs)
        (faction-icon faction identity)

        side
        (str " (" (tr-side side) ")")))
    (when-let [{:keys [wins losses]} (:run-info player)]
      [:span.standings wins "-" losses])]))

(defn- game-row
  [{:keys [title format room started players gameid current-game original-players] :as game}]
  (r/with-let [s (r/atom {:show-mod-menu false})
               user (:user @app-state)
               join (fn [action] (join-game gameid s action nil))]
    [:div.gameline {:class (when (= current-game gameid) "active")}
     (when (or (superuser? user)
               (and (:allow-spectator game)
                    (not current-game)))
       [:button {:on-click #(do (join "watch")
                                (resume-sound))} (tr [:lobby.watch "Watch"])])
     (when (and (not current-game)
                started
                (= 1 (count players))
                (some #(= (get-in % [:user :_id]) (get-in @app-state [:user :_id])) original-players))
       [:button {:on-click #(do (join "rejoin")
                                (resume-sound))}
        (tr [:lobby.rejoin "Rejoin"])])
     (let [c (:spectator-count game)]
       [:h4
        (str (when (:save-replay game) "🟢")
             (:title game)
             (when (pos? c) (str " (" (tr [:lobby.spectator-count] c) ")")))])

     [:div {:class "game-format"}
      [:span.format-label (tr [:lobby.format "Format"]) ":  "]
      [:span.format-type (tr-format (slug->format format "Unknown"))]]

     [:div (doall
             (map-indexed
               (fn [idx player]
                 ^{:key idx}
                 [player-view player game])
               original-players))]]))

(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [user game]
  (or (superuser? user)
      (let [players (get game :players [])
            blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
        (= -1 (.indexOf blocked-users (:username user))))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (if (= "tournament" (:room (first games)))
    games
    (let [blocked-games (filter #(blocked-from-game user %) games)
          blocked-users (get-in user [:options :blocked-users] [])]
      (filter #(blocking-from-game blocked-users %) blocked-games))))

(defn game-list [user {:keys [room games gameid]}]
  (let [roomgames (r/track (fn [] (filter #(= (:room %) room) @games)))
        filtered-games (r/track #(filter-blocked-games @user @roomgames))]
    [:div.game-list
     (if (empty? @filtered-games)
       [:h4 (tr [:angel-arena.no-games "No games"])]
       (let [get-player-wins (fn [game] (map #(get-wins (:run-info %)) (:players game)))
             groups (->> @filtered-games
                         (group-by #(apply max (get-player-wins %)))
                         (sort-by first >))]
         (doall
           (for [[wins games] groups]
             [:div.win-group {:key wins}
              [:div.win-divider {:key (str wins "-divider")} wins " " (tr [:angel-arena.wins "wins"])]
              (doall
                (for [game games]
                  ^{:key (:gameid game)}
                  [game-row (assoc game :current-game @gameid :password-game nil :editing false)]))]))))]))
