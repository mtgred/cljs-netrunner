(ns netrunner.gamelobby
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <!] :as async]
            [taoensso.sente  :as sente]
            [clojure.string :refer [join]]
            [netrunner.ajax :refer [GET]]
            [netrunner.ws :as ws]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [authenticated avatar] :as auth]
            [netrunner.gameboard :refer [init-game game-state toast launch-game parse-state]]
            [netrunner.cardbrowser :refer [image-url non-game-toast] :as cb]
            [netrunner.stats :refer [notnum->zero]]
            [netrunner.deckbuilder :refer [format-deck-status-span deck-status-span process-decks load-decks num->percent]]))

(def socket-channel (chan))

(defn resume-sound
  "Chrome doesn't allow audio until audio context is resumed (or created) after a user interaction."
  []
  (when-let [audio-context (aget js/Howler "ctx")]
    (.resume audio-context)))

(defn sort-games-list [games]
  (sort-by #(vec (map (assoc % :started (not (:started %))
                               :mygame (if-let [og (:originalPlayers %)]
                                         (some (fn [p] (= p (get-in @app-state [:user :_id])))
                                               (map (fn [g] (get-in g [:user :_id])) og))
                                         false))
                      [:started :date :mygame]))
           > games))

(ws/register-ws-handler!
  :games/list
  #(swap! app-state assoc :games (sort-games-list %)))

(ws/register-ws-handler!
  :games/diff
  (fn [{:keys [diff notification] :as msg}]
    (swap! app-state update-in [:games]
           (fn [games]
             (let [gamemap (into {} (map #(assoc {} (:gameid %) %) games))
                   create (merge gamemap (:create diff))
                   update (merge create (:update diff))
                   delete (apply dissoc update (keys (:delete diff)))]
               (sort-games-list (vals delete)))))
    (when (and notification (not (:gameid @app-state)))
      (.play (.getElementById js/document notification)))))

(ws/register-ws-handler!
  :lobby/select
  #(do (swap! app-state assoc :gameid (:gameid %))
       (when (:started %) (launch-game (parse-state (:state %))))))

(ws/register-ws-handler!
  :lobby/message
  (fn [{:keys [user text notification] :as msg}]
    (swap! app-state update-in [:messages] #(conj % msg))
    (when notification
      (.play (.getElementById js/document notification)))))

(ws/register-ws-handler!
  :lobby/timeout
  (fn [{:keys [gameid] :as msg}]
    (when (= gameid (:gameid @app-state))
      (non-game-toast "Game lobby closed due to inactivity" "error" {:time-out 0 :close-button true})
      (swap! app-state assoc :gameid nil))))

(go (while true
      (let [msg (<! socket-channel)]
        (case (:type msg)
          "game" (do (swap! app-state assoc :gameid (:gameid msg))
                     (when (:started msg) (launch-game nil)))
          "games" (do (when (:gamesdiff msg)
                        (swap! app-state update-in [:games]
                               (fn [games]
                                 (let [gamemap (into {} (map #(assoc {} (keyword (:gameid %)) %) games))
                                       create (merge gamemap (get-in msg [:gamesdiff :create]))
                                       update (merge create (get-in msg [:gamesdiff :update]))
                                       delete (apply dissoc update (map keyword (keys (get-in msg [:gamesdiff :delete]))))]
                                   (sort-games-list (vals delete))))))
                      (when (:games msg)
                        (swap! app-state assoc :games (sort-games-list (vals (:games msg)))))
                      (when-let [sound (:notification msg)]
                        (when-not (:gameid @app-state)
                          (.play (.getElementById js/document sound)))))
          "say" (do (swap! app-state update-in [:messages]
                           #(conj % {:user (:user msg) :text (:text msg)}))
                    (when-let [sound (:notification msg)]
                      (.play (.getElementById js/document sound ))))
          "start" (launch-game (:state msg))
          "Invalid password" (js/console.log "pwd" (:gameid msg))
          "lobby-notification" (toast (:text msg) (:severity msg) nil)
          nil))))

(defn send
  ([msg] (send msg nil))
  ([msg fn]
   (try (js/ga "send" "event" "lobby" msg) (catch js/Error e))
   ;(.emit socket "netrunner" (clj->js msg) fn)
   ))

(defn new-game [cursor owner]
  (authenticated
   (fn [user]
     (om/set-state! owner :title (str (:username user) "'s game"))
     (om/set-state! owner :side "Corp")
     (om/set-state! owner :editing true)
     (om/set-state! owner :flash-message "")
     (om/set-state! owner :protected false)
     (om/set-state! owner :password "")
     (om/set-state! owner :allowspectator true)
     (om/set-state! owner :spectatorhands false)
     (-> ".game-title" js/$ .select))))

(defn create-game [cursor owner]
  (authenticated
   (fn [user]
     (if (empty? (om/get-state owner :title))
       (om/set-state! owner :flash-message "Please fill a game title.")
       (if (and (om/get-state owner :protected)
                (empty? (om/get-state owner :password)))
        (om/set-state! owner :flash-message "Please fill a password")
        (do (om/set-state! owner :editing false)
            (swap! app-state assoc :messages [])
            (ws/ws-send! [:lobby/create
                          {:title          (om/get-state owner :title)
                           :password       (om/get-state owner :password)
                           :allowspectator (om/get-state owner :allowspectator)
                           :spectatorhands (om/get-state owner :spectatorhands)
                           :side           (om/get-state owner :side)
                           :room           (om/get-state owner :current-room)
                           :options        (:options @app-state)}])))))))

(defn join-game [gameid owner action password]
  (authenticated
   (fn [user]
     (om/set-state! owner :editing false)
     (swap! app-state assoc :messages [])
     (ws/ws-send! [(case action
                     "join" :lobby/join
                     "watch" :lobby/watch
                     "rejoin" :netrunner/rejoin)
                   {:gameid gameid :password password :options (:options @app-state)}]
                  8000
                  #(if (sente/cb-success? %)
                     (case %
                       403 (om/set-state! owner :error-msg "Invalid password")
                       404 (om/set-state! owner :error-msg "Not allowed")
                       200 (om/set-state! owner :prompt false))
                     (om/set-state! owner :error-msg "Connection aborted"))))))

(defn leave-lobby [cursor owner]
  (ws/ws-send! [:lobby/leave])
  (om/update! cursor :gameid nil)
  (om/update! cursor :message [])
  (om/set-state! owner :prompt false)
  (swap! app-state dissoc :password-gameid))

(defn leave-game []
  (ws/ws-send! [:netrunner/leave])
  (reset! game-state nil)
  (swap! app-state dissoc :gameid :side :password-gameid :win-shown)
  (.removeItem js/localStorage "gameid")
  (set! (.-onbeforeunload js/window) nil)
  (-> "#gameboard" js/$ .fadeOut)
  (-> "#gamelobby" js/$ .fadeIn))

(defn concede []
  (ws/ws-send! [:netrunner/concede]))

(defn send-msg [event owner]
  (.preventDefault event)
  (let [input (om/get-node owner "msg-input")
        text (.-value input)
        $div (js/$ ".lobby .message-list")]
    (when-not (empty? text)
      (ws/ws-send! [:lobby/say {:gameid (:gameid @app-state) :msg text}])
      (.scrollTop $div (+ (.prop $div "scrollHeight") 500))
      (aset input "value" "")
      (.focus input))))

(defn deckselect-modal [{:keys [gameid games decks sets user]} owner opts]
  (om/component
   (sab/html
    [:div.modal.fade#deck-select
     [:div.modal-dialog
      [:h3 "Select your deck"]
      [:div.deck-collection
       (let [players (:players (some #(when (= (:gameid %) gameid) %) games))
             side (:side (some #(when (= (:user %) user) %) players))]
         [:div {:data-dismiss "modal"}
          (for [deck (sort-by :date > (filter #(= (get-in % [:identity :side]) side) decks))]
            [:div.deckline {:on-click #(ws/ws-send! [:lobby/deck (:_id deck)])}
             [:img {:src (image-url (:identity deck))
                    :alt (get-in deck [:identity :title] "")}]
             [:div.float-right (deck-status-span sets deck)]
             [:h4 (:name deck)]
             [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
             [:p (get-in deck [:identity :title])]])])]]])))


(defn faction-icon
  [faction identity]
  (let [icon-span (fn [css-faction] [:span.faction-icon {:class css-faction :title identity}])]
    (case faction
      "Adam" (icon-span "adam")
      "Anarch" (icon-span "anarch")
      "Apex" (icon-span "apex")
      "Criminal" (icon-span "criminal")
      "Haas-Bioroid" (icon-span "hb")
      "Jinteki" (icon-span "jinteki")
      "NBN" (icon-span "nbn")
      "Shaper" (icon-span "shaper")
      "Sunny Lebeau" (icon-span "sunny")
      "Weyland Consortium" (icon-span "weyland")
      [:span.side "(Unknown)"])))

(defn user-status-span
  "Returns a [:span] showing players game completion rate"
  [player]
  (let [started (get-in player [:user :stats :games-started])
        completed (get-in player [:user :stats :games-completed])
        completion-rate (str (notnum->zero (num->percent completed started)) "%")
        completion-rate (if (< started 10) "Too little data" completion-rate)]
    [:span.user-status (get-in player [:user :username])
     [:div.status-tooltip.blue-shade
      [:div "Game Completion Rate: " completion-rate]]]))

(defn player-view [{:keys [player game] :as args}]
  (om/component
   (sab/html
    [:span.player
     (om/build avatar (:user player) {:opts {:size 22}})
     (user-status-span player)
     (let [side (:side player)
           faction (:faction (:identity (:deck player)))
           identity (:title (:identity (:deck player)))
           specs (:allowspectator game)]
       (cond
         (and (some? faction) (not= "Neutral" faction) specs) (faction-icon faction identity)
         side [:span.side (str "(" side ")")]))])))

(defn chat-view [messages owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (let [div (om/get-node owner "msg-list")
            height (.-scrollHeight div)]
        (when (< (- height (.-scrollTop div) (.height (js/$ ".lobby .chat-box"))) 500)
          (aset div "scrollTop" (.-scrollHeight div)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div.chat-box
        [:h3 "Chat"]
        [:div.message-list {:ref "msg-list"}
         (for [msg messages]
           (if (= (:user msg) "__system__")
             [:div.system (:text msg)]
             [:div.message
              (om/build avatar (:user msg) {:opts {:size 38}})
              [:div.content
               [:div.username (get-in msg [:user :username])]
               [:div (:text msg)]]]))]
        [:div
         [:form.msg-box {:on-submit #(send-msg % owner)}
          [:input {:ref "msg-input" :placeholder "Say something" :accessKey "l"}]
          [:button "Send"]]]]))))

(defn game-view [{:keys [title password started players gameid current-game password-game original-players editing] :as game} owner]
  (reify
    om/IRenderState
    (render-state [this state]
     (letfn [(join [action]
                (let [password (:password password-game password)]
                 (if (empty? password)
                  (join-game (if password-game (:gameid password-game) gameid) owner action nil)
                  (if-let [input-password (om/get-state owner :password)]
                    (join-game (if password-game (:gameid password-game) gameid) owner action input-password)
                    (do (swap! app-state assoc :password-gameid gameid) (om/set-state! owner :prompt action))))))]
       (sab/html
        [:div.gameline {:class (when (= current-game gameid) "active")}
         (when (and (:allowspectator game) (not (or password-game current-game editing)))
           [:button {:on-click #(do (join "watch") (resume-sound))} "Watch" editing])
         (when-not (or current-game editing (= (count players) 2) started password-game)
           [:button {:on-click #(do (join "join") (resume-sound))} "Join"])
         (when (and (not current-game) (not editing) started (not password-game)
                    (some #(= % (get-in @app-state [:user :_id]))
                          (map #(get-in % [:user :_id]) original-players)))
           [:button {:on-click #(do (join "rejoin") (resume-sound))} "Rejoin"])
         (let [c (count (:spectators game))]
           [:h4 (str (when-not (empty? (:password game))
                       "[PRIVATE] ")
                     (:title game)
                     (when (pos? c)
                       (str  " (" c " spectator" (when (> c 1) "s") ")")))])
         [:div (om/build-all player-view (map (fn [%] {:player % :game game}) (:players game)))]
         (when-let [prompt (om/get-state owner :prompt)]
           [:div.password-prompt
            [:h3 (str "Password for " (if password-game (:title password-game) title))]
            [:p
             [:input.game-title {:on-change #(om/set-state! owner :password (.. % -target -value))
                                 :type "password"
                                 :value (:password state) :placeholder "Password" :maxLength "30"}]]
            [:p
             [:button {:type "button" :on-click #(join prompt)}
              prompt]
             [:span.fake-link {:on-click #(do
                                            (swap! app-state dissoc :password-gameid)
                                            (om/set-state! owner :prompt false)
                                            (om/set-state! owner :error-msg nil)
                                            (om/set-state! owner :password nil))}
              "Cancel"]]
            (when-let [error-msg (om/get-state owner :error-msg)]
              [:p.flash-message error-msg])])])))))

(defn- blocked-from-game
  "Remove games for which the user is blocked by one of the players"
  [username game]
  (let [players (get game :players [])
    blocked-users (flatten (map #(get-in % [:user :options :blocked-users] []) players))]
    (= -1 (.indexOf blocked-users username))))

(defn- blocking-from-game
  "Remove games with players we are blocking"
  [blocked-users game]
  (let [players (get game :players [])
        player-names (map #(get-in % [:user :username] "") players)
        intersect (clojure.set/intersection (set blocked-users) (set player-names))]
    (empty? intersect)))

(defn filter-blocked-games
  [user games]
  (let [blocked-games (filter #(blocked-from-game (:username user) %) games)
        blocked-users (get-in user [:options :blocked-users] [])]
    (filter #(blocking-from-game blocked-users %) blocked-games)))

(defn game-list [{:keys [user games gameid password-game editing] :as cursor} owner]
  (let [roomgames (filter #(= (:room %) (om/get-state owner :current-room)) games)
        filtered-games (filter-blocked-games user roomgames)]
    [:div.game-list
     (if (empty? filtered-games)
       [:h4 "No games"]
       (for [game filtered-games]
        (om/build game-view (assoc game :current-game gameid :password-game password-game :editing editing))))]))

(def open-games-symbol "○")
(def closed-games-symbol "●")

(defn- room-tab
  "Creates the room tab for the specified room"
  [{:keys [user]} owner games room room-name]
  (let [room-games (filter #(= room (:room %)) games)
        filtered-games (filter-blocked-games user room-games)
        closed-games (count (filter #(:started %) filtered-games))
        open-games (- (count filtered-games) closed-games)]
    [:span.roomtab
     (if (= room (om/get-state owner :current-room))
       {:class "current"}
       {:on-click #(om/set-state! owner :current-room room)})
     room-name " (" open-games open-games-symbol " "
     closed-games closed-games-symbol ")"]))

(defn game-lobby [{:keys [games gameid messages sets user password-gameid] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:current-room "casual"})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:div.lobby-bg]
        [:div.container
         [:div.lobby.panel.blue-shade
          [:div.games
           [:div.button-bar
            (if (or gameid (:editing state))
              [:button.float-left {:class "disabled"} "New game"]
              [:button.float-left {:on-click #(do (new-game cursor owner) (resume-sound))} "New game"])
            [:div.rooms
             (room-tab cursor owner games "competitive" "Competitive")
             (room-tab cursor owner games "casual" "Casual")]]
           (let [password-game (some #(when (= password-gameid (:gameid %)) %) games)]
             (game-list (assoc cursor :password-game password-game :editing (:editing state)) owner))]

          [:div.game-panel
           (if (:editing state)
             [:div
              [:div.button-bar
               [:button {:type "button" :on-click #(create-game cursor owner)} "Create"]
               [:button {:type "button" :on-click #(om/set-state! owner :editing false)} "Cancel"]]

              (when-let [flash-message (:flash-message state)]
                [:p.flash-message flash-message])

              [:section
               [:h3 "Title"]
               [:input.game-title {:on-change #(om/set-state! owner :title (.. % -target -value))
                                   :value (:title state) :placeholder "Title" :maxLength "100"}]]

              [:section
               [:h3 "Side"]
               (for [option ["Corp" "Runner"]]
                 [:p
                  [:label [:input {:type "radio"
                                   :name "side"
                                   :value option
                                   :on-change #(om/set-state! owner :side (.. % -target -value))
                                   :checked (= (om/get-state owner :side) option)}] option]])]

              [:section
               [:h3 "Options"]
               [:p
                [:label
                 [:input {:type "checkbox" :checked (om/get-state owner :allowspectator)
                          :on-change #(om/set-state! owner :allowspectator (.. % -target -checked))}]
                 "Allow spectators"]]
               [:p
                [:label
                 [:input {:type "checkbox" :checked (om/get-state owner :spectatorhands)
                          :on-change #(om/set-state! owner :spectatorhands (.. % -target -checked))
                          :disabled (not (om/get-state owner :allowspectator))}]
                 "Make players' hidden information visible to spectators"]]
               [:div {:style {:display (if (om/get-state owner :spectatorhands) "block" "none")}}
                [:p "This will reveal both players' hidden information to ALL spectators of your game, "
                 "including hand and face-down cards."]
                [:p "We recommend using a password to prevent strangers from spoiling the game."]]
               [:p
                [:label
                 [:input {:type "checkbox" :checked (om/get-state owner :private)
                          :on-change #(let [checked (.. % -target -checked)]
                                        (om/set-state! owner :protected checked)
                                        (when (not checked) (om/set-state! owner :password "")))}]
                 "Password protected"]]
               (when (:protected state)
                 [:p
                  [:input.game-title {:on-change #(om/set-state! owner :password (.. % -target -value))
                                      :type "password"
                                      :value (:password state) :placeholder "Password" :maxLength "30"}]])]]

             (when-let [game (some #(when (= gameid (:gameid %)) %) games)]
               (let [players (:players game)]
                 [:div
                  [:div.button-bar
                   (when (= (-> players first :user) user)
                     (if (every? :deck players)
                       [:button {:on-click #(ws/ws-send! [:netrunner/start gameid])} "Start"]
                       [:button {:class "disabled"} "Start"]))
                   [:button {:on-click #(leave-lobby cursor owner)} "Leave"]
                   (when (= (-> players first :user) user)
                     [:button {:on-click #(ws/ws-send! [:lobby/swap gameid])} "Swap sides"])]
                  [:div.content
                   [:h2 (:title game)]
                   (when-not (every? :deck players)
                     [:div.flash-message "Waiting players deck selection"])
                   [:h3 "Players"]
                   [:div.players
                    (for [player (:players game)]
                      [:div
                       (om/build player-view {:player player})
                       (when-let [{:keys [_id name status] :as deck} (:deck player)]
                         [:span {:class (:status status)}
                          [:span.label
                           (if (= (:user player) user)
                             name
                             "Deck selected")]])
                       (when-let [deck (:deck player)]
                         [:div.float-right (format-deck-status-span (:status deck) true false)])
                       (when (= (:user player) user)
                         [:span.fake-link.deck-load
                          {:data-target "#deck-select" :data-toggle "modal"
                           :on-click (fn [] (send {:action "deck" :gameid (:gameid @app-state) :deck nil}))
                           } "Select deck"])])]
                   (when (:allowspectator game)
                     [:div.spectators
                      (let [c (count (:spectators game))]
                        [:h3 (str c " Spectator" (when (not= c 1) "s"))])
                      (for [spectator (:spectators game)]
                        (om/build player-view {:player spectator}))])]
                  (om/build chat-view messages {:state state})])))]
          (om/build deckselect-modal cursor)]]]))))

(om/root game-lobby app-state {:target (. js/document (getElementById "gamelobby"))})
