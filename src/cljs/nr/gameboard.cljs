(ns nr.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [differ.core :as differ]
            [jinteki.utils :refer [str->int]]
            [jinteki.cards :refer [all-cards]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [avatar] :as auth]
            [nr.cardbrowser :refer [add-symbols] :as cb]
            [nr.utils :refer [influence-dot map-longest toastr-options]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(defonce game-state (r/atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defonce board-dom (atom {}))
(defonce sfx-state (atom {}))

(defn image-url [{:keys [side code] :as card}]
  (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)]))
        art-options (:alt_art (get (:alt-arts @app-state) code))
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art-available (and art-options (not-empty art-options))
        has-art (and art-options
                     art
                     (contains? art-options (keyword art)))
        version-path (if (and has-art show-art)
                       (get art-options (keyword art) (:code card))
                       (:code card))]
    (str "/img/cards/" version-path ".png")))

(defn get-side [state]
  (let [user-id (:_id (:user @app-state))]
    (cond
      (= (get-in state [:runner :user :_id]) user-id) :runner
      (= (get-in state [:corp :user :_id]) user-id) :corp
      :else :spectator)))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

(defn init-game [state]
  (let [side (get-side state)]
    (.setItem js/localStorage "gameid" (:gameid @app-state))
    (reset! game-state state)
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)
    (reset! lock false)))

(defn launch-game [{:keys [state]}]
  (prn "launching game")
  (init-game state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(declare toast)

(defn notify
  "Send a notification to the chat, and a toast to the current player of the specified severity"
  [text severity]
  (swap! game-state update-in [:log] #(conj % {:user "__system__" :text text}))
  (toast text severity nil))

(def zoom-channel (chan))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! lock false))))

(defn handle-state [{:keys [state]}] (init-game state))

(defn handle-diff [{:keys [gameid diff]}]
  (when (= gameid (:gameid @game-state))
    (swap! game-state #(differ/patch @last-state diff))
    (check-lock?)
    (reset! last-state @game-state)))

(defn handle-timeout [{:keys [gameid]}]
  (when (= gameid (:gameid @game-state))
    (toast "Game closed due to inactivity" "error" {:time-out 0 :close-button true})))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(ws/register-ws-handler! :netrunner/state #(handle-state (parse-state %)))
(ws/register-ws-handler! :netrunner/start #(launch-game (parse-state %)))
(ws/register-ws-handler! :netrunner/diff #(handle-diff (parse-state %)))
(ws/register-ws-handler! :netrunner/timeout #(handle-timeout (parse-state %)))

(def anr-icons {"[Credits]" "credit"
                "[$]" "credit"
                "[c]" "credit"
                "[Credit]" "credit"
                "[Click]" "click"
                "[Subroutine]" "subroutine"
                "[Recurring Credits]" "recurring-credit"
                "1[Memory Unit]" "mu1"
                "1[mu]" "mu1"
                "2[Memory Unit]" "mu2"
                "2[mu]" "mu2"
                "3[Memory Unit]" "mu3"
                "3[mu]" "mu3"
                "[Link]" "link"
                "[l]" "link"
                "[Memory Unit]" "mu"
                "[mu]" "mu"
                "[Trash]" "trash"
                "[t]" "trash"})

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock] :as args}]
   (when (or (not @lock) no-lock)
     (try (js/ga "send" "event" "game" command) (catch js/Error e))
     (when-not no-lock (reset! lock true))
     (ws/ws-send! [:netrunner/action {:gameid-str (:gameid @game-state)
                                      :command command
                                      :args args}]))))

(defn send-msg [s]
  (let [input (:msg-input @s)
        text (:msg @s)]
    (when-not (empty? text)
      (ws/ws-send! [:netrunner/say {:gameid-str (:gameid @game-state)
                                    :msg text}])
      (let [msg-list (:message-list @board-dom)]
        (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
      (swap! s assoc :msg "")
      ;; don't try to focus for / commands
      (when input (.focus input)))))

(defn send-typing [s]
  "Send a typing event to server for this user if it is not already set in game state"
  (let [input (:msg-input @s)
        text (:msg @s)]
    (if (empty? text)
      (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                       :typing false}])
      (when (not-any? #{(get-in @app-state [:user :username])} (:typing @game-state))
        (ws/ws-send! [:netrunner/typing {:gameid-str (:gameid @game-state)
                                         :typing true}])))))

(defn mute-spectators [mute-state]
  (ws/ws-send! [:netrunner/mute-spectators {:gameid-str (:gameid @game-state)
                                            :mute-state mute-state}]))

(defn concede []
  (ws/ws-send! [:netrunner/concede {:gameid-str (:gameid @game-state)}]))

(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/mtgred/netrunner/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" type) "error" type))]
    (f (if (= "exception" type) (build-exception-msg msg (:last-error @game-state)) msg))
    (send-command "toast")))

(defn action-list [{:keys [type zone rezzed advanceable advance-counter advancementcost current-cost] :as card}]
  (-> []
      (#(if (or (and (= type "Agenda")
                     (#{"servers" "onhost"} (first zone)))
                (= advanceable "always")
                (and rezzed
                     (= advanceable "while-rezzed"))
                (and (not rezzed)
                     (= advanceable "while-unrezzed")))
          (cons "advance" %) %))
      (#(if (and (= type "Agenda") (>= advance-counter current-cost))
          (cons "score" %) %))
      (#(if (#{"Asset" "ICE" "Upgrade"} type)
          (if-not rezzed (cons "rez" %) (cons "derez" %))
          %))))

(defn handle-abilities [{:keys [abilities facedown side type] :as card} c-state]
  (let [actions (action-list card)
        c (+ (count actions) (count abilities))]
    (when-not (and (= side "Runner") facedown)
      (cond

        ;; Toggle abilities panel
        (or (> c 1)
            (some #{"derez" "advance"} actions)
            (and (= type "ICE")
                 (not (:run @game-state))))
        (if (:abilities @c-state)
          (swap! c-state dissoc :abilities)
          (swap! c-state assoc :abilities true))

        ;; Trigger first (and only) ability / action
        (= c 1)
        (if (= (count abilities) 1)
          (send-command "ability" {:card card :ability 0})
          (send-command (first actions) {:card card}))))))

(defn handle-card-click [{:keys [type zone root] :as card} c-state]
  (let [side (:side @game-state)]
    (when (not-spectator?)
      (cond
        ;; Selecting card
        (= (get-in @game-state [side :prompt 0 :prompt-type]) "select")
        (send-command "select" {:card card})

        ;; Card is an identity of player's side
        (and (= (:type card) "Identity")
             (= side (keyword (.toLowerCase (:side card)))))
        (handle-abilities card c-state)

        ;; Runner side
        (= side :runner)
        (case (first zone)
          "hand" (if (:host card)
                   (when (:installed card)
                     (handle-abilities card c-state))
                   (send-command "play" {:card card}))
          ("rig" "current" "onhost" "play-area") (handle-abilities card c-state)
          ("servers") (when (and (= type "ICE") (:rezzed card))
                        ;; ICE that should show list of abilities that send messages to fire sub
                        (if (:runner-abilities @c-state)
                          (swap! c-state dissoc :runner-abilities)
                          (swap! c-state assoc :runner-abilities true)))
          nil)

        ;; Corp side
        (= side :corp)
        (case (first zone)
          "hand" (case type
                   ("Upgrade" "ICE") (if root
                                       (send-command "play" {:card card :server root})
                                       (if (:servers @c-state)
                                         (swap! c-state dissoc :servers)
                                         (swap! c-state assoc :servers true)))
                   ("Agenda" "Asset") (if (< (count (get-in @game-state [:corp :servers])) 4)
                                        (send-command "play" {:card card :server "New remote"})
                                        (if (:servers @c-state)
                                          (swap! c-state dissoc :servers)
                                          (swap! c-state assoc :servers true)))
                   (send-command "play" {:card card}))
          ("servers" "scored" "current" "onhost") (handle-abilities card c-state)
          "rig" (when (:corp-abilities card)
                  (if (:corp-abilities @c-state)
                    (swap! c-state dissoc :corp-abilities)
                    (swap! c-state assoc :corp-abilities true)))
          nil)))))

(defn in-play? [card]
  (let [dest (when (= (:side card) "Runner")
               (get-in @game-state [:runner :rig (keyword (.toLowerCase (:type card)))]))]
    (some #(= (:title %) (:title card)) dest)))

(defn has?
  "Checks the string property of the card to see if it contains the given value"
  [card property value]
  (when-let [p (property card)]
    (> (.indexOf p value) -1)))

(defn has-subtype?
  "Checks if the specified subtype is present in the card."
  [card subtype]
  (or (has? card :subtype subtype)
      (when-let [persistent-subs (-> card :persistent :subtype)]
        (includes? persistent-subs subtype))))

(defn playable? [{:keys [title side zone cost type uniqueness abilities] :as card}]
  (let [my-side (:side @game-state)
        me (my-side @game-state)]
    (and (= (keyword (.toLowerCase side)) my-side)

         (cond

           (has-subtype? card "Double")
           (if (>= (:click me) 2) true false)

           (has-subtype? card "Triple")
           (if (>= (:click me) 3) true false)

           (= (:code card) "07036") ; Day Job
           (if (>= (:click me) 4) true false)

           (has-subtype? card "Priority")
           (if (get-in @game-state [my-side :register :spent-click]) false true)

           :else
           true)

         (and (= zone ["hand"])
              (or (not uniqueness) (not (in-play? card)))
              (or (#{"Agenda" "Asset" "Upgrade" "ICE"} type) (>= (:credit me) cost))
              (pos? (:click me))))))

(defn spectator-view-hidden?
  "Checks if spectators are allowed to see hidden information, such as hands and face-down cards"
  []
  (and (get-in @game-state [:options :spectatorhands])
       (not (not-spectator?))))

(def ci-open "\u2664")
(def ci-seperator "\u2665")
(def ci-close "\u2666")

(defn is-card-item [item]
  (and (> (.indexOf item ci-seperator) -1)
       (= 0 (.indexOf item ci-open))))

(defn extract-card-info [item]
  (if (is-card-item item)
    [(.substring item 1 (.indexOf item ci-seperator))
     (.substring item (inc (.indexOf item ci-seperator)) (dec (count item)))]))

(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr]
    (if (= "[!]" item)
      [:div.smallwarning "!"]
      (if-let [class (anr-icons item)]
        [:span {:class (str "anr-icon " class) :key class}]
        (if-let [[title code cid] (extract-card-info item)]
          [:span {:class "fake-link" :id code :key title} title]
          [:span {:key item} item])))))

(defn get-non-alt-art [[title cards]]
  {:title title :code (:code (first cards))})

(defn prepare-cards []
  (->> @all-cards
       (filter #(not (:replaced_by %)))
       (group-by :title)
       (map get-non-alt-art)
       (sort-by #(count (:title %1)))
       (reverse)))

(def prepared-cards (memoize prepare-cards))

(def create-span (memoize create-span-impl))

(defn find-card-regex-impl [title]
  (str "(^|[^" ci-open "\\S])" title "(?![" ci-seperator "\\w]|([^" ci-open "]+" ci-close "))"))

(def find-card-regex (memoize find-card-regex-impl))

(defn card-image-token-impl [title code]
  (str "$1" ci-open title ci-seperator code ci-close))

(def card-image-token (memoize card-image-token-impl))

(defn card-image-reducer [text card]
  (.replace text (js/RegExp. (find-card-regex (:title card)) "g") (card-image-token (:title card) (:code card))))

(defn add-image-codes-impl [text]
  (reduce card-image-reducer text (prepared-cards)))

(def add-image-codes (memoize add-image-codes-impl))

(defn get-message-parts-impl [text]
  (let [with-image-codes (add-image-codes (if (nil? text) "" text))
        splitted (.split with-image-codes (js/RegExp. (str "(" ci-open "[^" ci-close "]*" ci-close ")") "g"))
        oldstyle (for [i splitted]
                   (seq (.split i (js/RegExp. (str "([1-3]\\[mu\\]|\\[[^\\]]*\\])") "g"))))]
    (flatten oldstyle)))

(def get-message-parts (memoize get-message-parts-impl))

(defn get-card-code [e]
  (let [code (str (.. e -target -id))]
    (when (pos? (count code))
      code)))

(defn card-preview-mouse-over [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (when-let [card (some #(when (= (:code %) code) %) @all-cards)]
      (put! channel (assoc card :implementation :full))))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (put! channel false))
  nil)

(defn log-pane []
  (let [s (r/atom {})
        log (r/cursor game-state [:log])
        typing (r/cursor game-state [:typing])
        gameid (r/cursor game-state [:gameid])
        games (r/cursor app-state [:games])]

    (r/create-class
      {:display-name "log-pane"

       :component-did-mount
       (fn []
         (-> ".log" js/$ (.resizable #js {:handles "w"})))

       :component-did-update
       (fn []
         (let [div (:message-list @board-dom)
               scrolltop (.-scrollTop div)
               height (.-scrollHeight div)]
           (when (or (zero? scrolltop)
                     (< (- height scrolltop (.height (js/$ ".gameboard .log"))) 500))
             (set! (.-scrollTop div) height))))

       :reagent-render
       (fn []
        [:div.log {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                   :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
         [:div.panel.blue-shade.messages {:ref #(swap! board-dom assoc :message-list %)}
          (doall (map-indexed
                   (fn [i msg]
                     (when-not (and (= (:user msg) "__system__") (= (:text msg) "typing"))
                       (if (= (:user msg) "__system__")
                         [:div.system {:key i} (map-indexed (fn [i item] [:<> {:key i} (create-span item)])
                                                            (get-message-parts (:text msg)))]
                         [:div.message {:key i}
                          [avatar (:user msg) {:opts {:size 38}}]
                          [:div.content
                           [:div.username (get-in msg [:user :username])]
                           [:div (doall
                                   (map-indexed (fn [i item] [:<> {:key i} (create-span item)])
                                                (get-message-parts (:text msg))))]]])))
                   @log))]
         (when (seq (remove nil? (remove #{(get-in @app-state [:user :username])} @typing)))
           [:div [:p.typing (for [i (range 10)] ^{:key i} [:span " " influence-dot " "])]])
         (if-let [game (some #(when (= @gameid (str (:gameid %))) %) @games)]
           (when (or (not-spectator?)
                     (not (:mutespectators game)))
             [:form {:on-submit #(do (.preventDefault %)
                                     (send-msg s))
                     :on-input #(do (.preventDefault %)
                                    (send-typing s))}
              [:input {:ref #(swap! board-dom assoc :msg-input %) :placeholder "Say something" :accessKey "l"
                       :value (:msg @s)
                       :on-change #(swap! s assoc :msg (-> % .-target .-value))}]]))])})))

(defn handle-dragstart [e card]
  (-> e .-target js/$ (.addClass "dragged"))
  (-> e .-dataTransfer (.setData "card" (.stringify js/JSON (clj->js card)))))

(defn handle-drop [e server]
  (-> e .-target js/$ (.removeClass "dragover"))
  (let [card (-> e .-dataTransfer (.getData "card") ((.-parse js/JSON)) (js->clj :keywordize-keys true))
        side (if (#{"HQ" "R&D" "Archives"} server) "Corp" "Runner")]
    (send-command "move" {:card card :server server})))

(defn abs [n] (max n (- n)))

;; touch support
(defonce touchmove (atom {}))

(defn release-touch [card]
  (-> card (.removeClass "disable-transition"))
  (-> card (.css "position" ""))
  (-> card (.css "top" "")))

(defn update-card-position [card touch]
  (-> card (.css "left" (str (- (int (aget touch "pageX")) 30) "px")))
  (-> card (.css "top"  (str (- (int (aget touch "pageY")) 42) "px"))))

(defn get-card [e server]
  (-> e .-target js/$ (.closest ".card-wrapper")))

(defn get-server-from-touch [touch]
  (let [cX (.. touch -clientX)
        cY (.. touch -clientY)
        server (-> (js/document.elementFromPoint cX cY)
                   js/$
                   (.closest "[data-server]")
                   (.attr "data-server"))]
    [server (> (+ (abs (- (:x @touchmove) cX))
                  (abs (- (:y @touchmove) cY)))
               30)]))

(defn handle-touchstart [e cursor]
  (let [touch (aget (.. e -targetTouches) 0)
        [server _] (get-server-from-touch touch)
        card (get-card e server)]
    (-> card (.addClass "disable-transition"))
    (reset! touchmove {:card (.stringify js/JSON (clj->js @cursor))
                       :x (.. touch -clientX)
                       :y (.. touch -clientY)
                       :start-server server})))

(defn handle-touchmove [e]
  (let [touch (aget (.. e -targetTouches) 0)
        card (get-card e (:start-server @touchmove))]
    (-> card (.css "position" "fixed"))
    (update-card-position card touch)))

(defn handle-touchend [e]
  (let [touch (aget (.. e -changedTouches) 0)
        card (get-card e (:start-server @touchmove))
        [server moved-enough] (get-server-from-touch touch)]
    (release-touch card)
    (when (and server moved-enough (not= server (:start-server @touchmove)))
      (let [cardinfo (-> @touchmove :card ((.-parse js/JSON)) (js->clj :keywordize-keys true))]
        (send-command "move" {:card cardinfo :server server})))))

(defn ability-costs [ab]
  (when-let [cost (:cost ab)]
    (str (clojure.string/join
           ", " (for [c (partition 2 cost)]
                  (str (case (first c)
                         "credit" (str (second c) " [" (capitalize (name (first c))) "]")
                         (clojure.string/join "" (repeat (second c) (str "[" (capitalize (name (first c))) "]"))))))) ": ")))

(defn remote->num [server]
  (-> server str (clojure.string/split #":remote") last str->int))

(defn remote->name [server]
  (let [num (remote->num server)]
    (str "Server " num)))

(defn central->name [zone]
  "Converts a central zone keyword to a string."
  (case (if (keyword? zone) zone (last zone))
    :hq "HQ"
    :rd "R&D"
    :archives "Archives"
    nil))

(defn zone->name [zone]
  "Converts a zone to a string."
  (or (central->name zone)
      (remote->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (str->int
      (last (clojure.string/split (str zone) #":remote")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn get-remotes [servers]
  (->> servers
       (filter #(not (#{:hq :rd :archives} (first %))))
       (sort-by #(zone->sort-key (first %)))))

(defn remote-list [remotes]
  (->> remotes (map first) zones->sorted-names))

(defn card-counter-type [card]
  (let [counter-type (:counter-type card)]
    ;; Determine the appropriate type of counter for styling, falling back to
    ;; power counters when no other type can be inferred.
    (cond
      ;; If an installed card contains an annotation, use it.
      (and (:installed card)
           (not (nil? counter-type)))
      counter-type
      (= "Agenda" (:type card)) "Agenda"
      ;; Assume uninstalled cards with counters are hosted on Personal
      ;; Workshop.
      (not (:installed card)) "Power"
      (not (:subtype card)) "Power"
      (> (.indexOf (:subtype card) "Virus") -1) "Virus"
      :else "Power")))

(defn facedown-card
  "Image element of a facedown card"
  ([side] (facedown-card side [] nil))
  ([side class-list alt-alt-text]
   (let [s (.toLowerCase side)
         alt (if (nil? alt-alt-text)
               (str "Facedown " s " card")
               alt-alt-text)
         tag (->> class-list
                  vec
                  (concat ["img" "card"])
                  (join ".")
                  keyword)]
     [tag {:src (str "/img/" s ".png")
           :alt alt}])))

(defn card-img
  "Build an image of the card (is always face-up). Only shows the zoomed card image, does not do any interaction."
  [{:keys [code title] :as card}]
  (when code
    [:div.card-frame
     [:div.blue-shade.card {:on-mouse-enter #(put! zoom-channel card)
                            :on-mouse-leave #(put! zoom-channel false)}
      (when-let [url (image-url card)]
        [:div
         [:span.cardname title]
         [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]])]]))

(defn face-down?
  "Returns true if the installed card should be drawn face down."
  [{:keys [side type facedown rezzed host] :as card}]
  (if (= side "Corp")
    (and (not= type "Operation")
         (not rezzed)
         (not= (:side host) "Runner"))
    facedown))

(defn card-zoom [card]
  [:div.card-preview.blue-shade
   [:h4 (:title card)]
   (when-let [memory (:memoryunits card)]
     (if (< memory 3)
       [:div.anr-icon {:class (str "mu" memory)} ""]
       [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
   (when-let [cost (:cost card)]
     [:div.heading (str "Cost: " cost)])
   (when-let [trash-cost (:trash card)]
     [:div.heading (str "Trash cost: " trash-cost)])
   (when-let [strength (:strength card)]
     [:div.heading (str "Strength: " strength)])
   (when-let [requirement (:advancementcost card)]
     [:div.heading (str "Advancement requirement: " requirement)])
   (when-let [agenda-point (:agendatpoints card)]
     [:div.heading (str "Agenda points: " agenda-point)])
   (when-let [min-deck-size (:minimumdecksize card)]
     [:div.heading (str "Minimum deck size: " min-deck-size)])
   (when-let [influence-limit (:influencelimit card)]
     [:div.heading (str "Influence limit: " influence-limit)])
   (when-let [influence (:factioncost card)]
     (when-let [faction (:faction card)]
       [:div.heading "Influence "
        [:span.influence
         {:dangerouslySetInnerHTML #js {:__html (apply str (for [i (range influence)] "&#8226;"))}
          :class                   (-> faction .toLowerCase (.replace " " "-"))}]]))
   [:div.text
    [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                          "" (str ": " (:subtype card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text card))}}]]
   (when-let [url (image-url card)]
     [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])

(defn runner-abs [card c-state runner-abilities subroutines title]
  [:div.panel.blue-shade.runner-abilities {:style (when (:runner-abilities @c-state) {:display "inline"})}
   (map-indexed
     (fn [i ab]
       [:div {:key i
              :on-click #(do (send-command "runner-ability" {:card card
                                                             :ability i}))
              :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}])
     runner-abilities)
   (when (> (count subroutines) 1)
     [:div {:on-click #(send-command "system-msg"
                                     {:msg (str "indicates to fire all subroutines on " title)})}
      "Let all subroutines fire"])
   (map-indexed
     (fn [i sub]
       [:div {:key i
              :on-click #(send-command "system-msg"
                                       {:msg (str "indicates to fire the \"" (:label sub)
                                                  "\" subroutine on " title)})
              :dangerouslySetInnerHTML #js {:__html (add-symbols (str "Let fire: \"" (:label sub) "\""))}}])
     subroutines)])

(defn corp-abs [card c-state corp-abilities]
    [:div.panel.blue-shade.corp-abilities {:style (when (:corp-abilities @c-state) {:display "inline"})}
     (map-indexed
       (fn [i ab]
         [:div {:on-click #(do (send-command "corp-ability" {:card card
                                                             :ability i}))
                :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}])
       corp-abilities)])

(defn server-menu [card c-state remotes type zone]
  (let [centrals ["Archives" "R&D" "HQ"]
        remotes (concat (remote-list remotes) ["New remote"])
        servers (case type
                  ("Upgrade" "ICE") (concat centrals remotes)
                  ("Agenda" "Asset") remotes)]
   [:div.panel.blue-shade.servers-menu {:style (when (:servers @c-state) {:display "inline"})}
     (map-indexed
       (fn [i label]
         [:div {:key i
                :on-click #(do (send-command "play" {:card card :server label})
                               (swap! c-state dissoc :servers))}
          label])
       servers)]))

(defn card-abilities [card c-state abilities subroutines]
  (let [actions (action-list card)
        dynabi-count (count (filter :dynamic abilities))]
    (when (or (> (+ (count actions) (count abilities) (count subroutines)) 1)
              (some #{"derez" "rez" "advance"} actions)
              (= type "ICE"))
      [:div.panel.blue-shade.abilities {:style (when (:abilities @c-state) {:display "inline"})}
        (map-indexed
          (fn [i action]
            [:div {:key i
                   :on-click #(do (send-command action {:card card}))} (capitalize action)])
          actions)
       (map-indexed
         (fn [i ab]
           (if (:dynamic ab)
             [:div {:key i
                    :on-click #(do (send-command "dynamic-ability" (assoc (select-keys ab [:dynamic :source :index])
                                                                     :card card)))
                    :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]
             [:div {:key i
                    :on-click #(do (send-command "ability" {:card card
                                                            :ability (- i dynabi-count)})
                                   (swap! c-state dissoc :abilities))
                    :dangerouslySetInnerHTML #js {:__html (add-symbols (str (ability-costs ab) (:label ab)))}}]))
         abilities)
       (map-indexed
         (fn [i sub]
           [:div {:key i
                  :on-click #(do (send-command "subroutine" {:card card :subroutine i})
                                 (swap! c-state dissoc :abilities))
                  :dangerouslySetInnerHTML #js {:__html (add-symbols (str "[Subroutine]" (:label sub)))}}])
         subroutines)])))

(defn card-view [{:keys [zone code type abilities counter advance-counter advancementcost current-cost subtype
                         advanceable rezzed strength current-strength title remotes selected hosted
                         side rec-counter facedown server-target subtype-target icon new runner-abilities subroutines
                         corp-abilities]
                  :as card}
                 flipped]
  (r/with-let [c-state (r/atom {})]
    [:div.card-frame
     [:div.blue-shade.card {:class (str (when selected "selected") (when new " new"))
                            :draggable (when (not-spectator?) true)
                            :on-touch-start #(handle-touchstart % card)
                            :on-touch-end   #(handle-touchend %)
                            :on-touch-move  #(handle-touchmove %)
                            :on-drag-start #(handle-dragstart % card)
                            :on-drag-end #(-> % .-target js/$ (.removeClass "dragged"))
                            :on-mouse-enter #(when (or (not (or (not code) flipped facedown))
                                                       (spectator-view-hidden?)
                                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                                               (put! zoom-channel card))
                            :on-mouse-leave #(put! zoom-channel false)
                            :on-click #(handle-card-click card c-state)}
      (when-let [url (image-url card)]
        (if (or (not code) flipped facedown)
          (let [facedown-but-known (or (not (or (not code) flipped facedown))
                                       (spectator-view-hidden?)
                                       (= (:side @game-state) (keyword (.toLowerCase side))))
                alt-str (if facedown-but-known (str "Facedown " title) nil)]
            [facedown-card side ["bg"] alt-str])
          [:div
           [:span.cardname title]
           [:img.card.bg {:src url :alt title :onError #(-> % .-target js/$ .hide)}]]))
      [:div.counters
       (when counter
         (map-indexed (fn [i [type num-counters]]
                (when (pos? num-counters)
                  (let [selector (str "div.darkbg." (lower-case (name type)) "-counter.counter")]
                    [(keyword selector) {:key type} num-counters])))
              counter))
       (when (pos? rec-counter) [:div.darkbg.recurring-counter.counter {:key "rec"} rec-counter])
       (when (pos? advance-counter) [:div.darkbg.advance-counter.counter {:key "adv"} advance-counter])]
      (when (and current-strength (not= strength current-strength))
        current-strength [:div.darkbg.strength current-strength])
      (when (get-in card [:special :extra-subs])
        [:div.darkbg.extra-subs \+])
      (when-let [{:keys [char color]} icon] [:div.darkbg.icon {:class color} char])
      (when server-target [:div.darkbg.server-target server-target])
      (when subtype-target
        (let [colour-type (case subtype-target
                            ("Barrier" "Sentry") (lower-case subtype-target)
                            "Code Gate" "code-gate"
                            nil)
              label (if (includes? subtype-target " - ")
                      (->> (split subtype-target #" - ")
                           (map first)
                           (join " - "))
                      subtype-target)]
          [:div.darkbg.subtype-target {:class colour-type} label]))

      (when (and (= zone ["hand"]) (#{"Agenda" "Asset" "ICE" "Upgrade"} type))
        [server-menu card c-state remotes type zone])

      (when (pos? (+ (count runner-abilities) (count subroutines)))
        [runner-abs card c-state runner-abilities subroutines title])

      (when (pos? (count corp-abilities))
        [corp-abs card c-state corp-abilities])

      [card-abilities card c-state abilities subroutines]

      (when (#{"servers" "onhost"} (first zone))
        (cond
          (and (= type "Agenda") (>= advance-counter (or current-cost advancementcost)))
          [:div.panel.blue-shade.menu.abilities
           [:div {:on-click #(send-command "advance" {:card card})} "Advance"]
           [:div {:on-click #(send-command "score" {:card card})} "Score"]]
          (or (= advanceable "always") (and rezzed (= advanceable "rezzed-only")))
          [:div.panel.blue-shade.menu.abilities
           [:div {:on-click #(send-command "advance" {:card card})} "Advance"]
           [:div {:on-click #(send-command "rez" {:card card})} "Rez"]]))]
     (when (pos? (count hosted))
       [:div.hosted
        (doall
          (for [card hosted]
            ^{:key (:cid card)}
            (let [flipped (face-down? card)]
              [card-view card flipped])))])]))

(defn drop-area [server hmap]
  (merge hmap {:on-drop #(handle-drop % server)
               :on-drag-enter #(-> % .-target js/$ (.addClass "dragover"))
               :on-drag-leave #(-> % .-target js/$ (.removeClass "dragover"))
               :on-drag-over #(.preventDefault %)
               :data-server server}))

(defn close-popup [event ref msg shuffle? deck?]
  (-> ref js/$ .fadeOut)
  (cond
    shuffle? (send-command "shuffle" {:close "true"})
    deck? (send-command "close-deck")
    msg (send-command "system-msg" {:msg msg}))
  (.stopPropagation event))

(defn label [cursor opts]
  (let [fn (or (:fn opts) count)]
    [:div.header {:class (when (> (count cursor) 0) "darkbg")}
     (str (:name opts) " (" (fn cursor) ")")]))

(defn- this-user?
  [player]
  (= (-> player :user :_id) (-> @app-state :user :_id)))

(defn build-hand-card-view
  [player remotes wrapper-class]
  (let [side (get-in @player [:identity :side])]
    (fn [player remotes wrapper-class]
      (let [size (count (:hand @player))]
        [:div
         (doall (map-indexed
            (fn [i card]
              [:div {:key (:cid card)
                     :class (str
                              (if (and (not= "select" (get-in @player [:prompt 0 :prompt-type]))
                                       (this-user? player)
                                       (not (:selected card)) (playable? card))
                                "playable" "")
                              " "
                              wrapper-class)
                     :style {:left (* (/ 320 (dec size)) i)}}
               (if (or (this-user? player)
                       (:openhand @player)
                       (spectator-view-hidden?))
                 [card-view (assoc card :remotes remotes)]
                 [facedown-card side])])
            (:hand @player)))]))))

(defn hand-view [player remotes popup popup-direction]
  (let [s (r/atom {})
        side (get-in @player [:identity :side])
        name (if (= side "Corp") "HQ" "Grip")]
    (fn [player remotes popup popup-direction]
      (let [size (count (:hand @player))]
        [:div.hand-container
         [:div.hand-controls
          [:div.panel.blue-shade.hand
           (drop-area name {:class (when (> size 6) "squeeze")})
           [build-hand-card-view player remotes "card-wrapper"]
           [label (:hand player) {:opts {:name name}}]]
          (when popup
            [:div.panel.blue-shade.hand-expand
             {:on-click #(-> (:hand-popup @s) js/$ .fadeToggle)}
             "+"])]
         (when popup
           [:div.panel.blue-shade.popup {:ref #(swap! s assoc :hand-popup %) :class popup-direction}
            [:div
             [:a {:on-click #(close-popup % (:hand-popup @s) nil false false)} "Close"]
             [:label (str size " card" (when (not= 1 size) "s") ".")]
             [build-hand-card-view player remotes "card-popup-wrapper"]]])]))))

(defn show-deck [event ref]
  (-> ((keyword (str ref "-content")) @board-dom) js/$ .fadeIn)
  (-> ((keyword (str ref "-menu")) @board-dom) js/$ .fadeOut)
  (send-command "view-deck"))

(defn identity-view [player]
  [:div.blue-shade.identity
   [card-view (:identity player)]])

(defn deck-view [{:keys [identity deck] :as player}]
   (let [is-runner (= "Runner" (:side identity))
         side (if is-runner :runner :corp)
         name (if is-runner "Stack" "R&D")
         ref (if is-runner "stack" "rd")
         menu-ref (keyword (str ref "-menu"))
         content-ref (keyword (str ref "-content"))]
     (fn [{:keys [identity deck] :as player}]
       [:div.blue-shade.deck
        (drop-area name {:on-click #(-> (menu-ref @board-dom) js/$ .toggle)})
        (when (pos? (count deck))
          [facedown-card (:side identity) ["bg"] nil])
        [label deck {:opts {:name name}}]
        (when (= (:side @game-state) side)
          [:div.panel.blue-shade.menu {:ref #(swap! board-dom assoc menu-ref %)}
           [:div {:on-click #(do (send-command "shuffle")
                                 (-> (menu-ref @board-dom) js/$ .fadeOut))} "Shuffle"]
           [:div {:on-click #(show-deck % ref)} "Show"]])
        (when (= (:side @game-state) side)
          [:div.panel.blue-shade.popup {:ref #(swap! board-dom assoc content-ref %)}
           [:div
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" false true)}
             "Close"]
            [:a {:on-click #(close-popup % (content-ref @board-dom) "stops looking at their deck" true true)}
             "Close & Shuffle"]]
           (doall
             (for [card deck]
               ^{:key (:cid card)}
               [card-view card]))])])))

(defmulti discard-view #(get-in % [:identity :side]))

(defmethod discard-view "Runner" [{:keys [discard]}]
  (let [s (r/atom {})]
    (fn [{:keys [discard]}]
      [:div.blue-shade.discard
       (drop-area "Heap" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
       (when-not (empty? discard)
         [:<> [card-view (last discard)]])
       [label discard {:opts {:name "Heap"}}]
       [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                     :class (if (= (:side @game-state) :runner) "me" "opponent")}
        [:div
         [:a {:on-click #(close-popup % (:popup @s) nil false false)} "Close"]]
        (doall
          (for [card discard]
            ^{:key (:cid card)}
            [card-view card]))]])))

(defmethod discard-view "Corp" [{:keys [discard servers]}]
  (let [s (r/atom {})]
    (fn [{:keys [discard servers]}]
      (let [faceup? #(or (:seen %) (:rezzed %))
            draw-card #(if (faceup? %)
                         [card-view %]
                         (if (or (= (:side @game-state) :corp)
                                 (spectator-view-hidden?))
                           [:div.unseen [card-view %]]
                           [facedown-card "corp"]))]
        [:div.blue-shade.discard
         (drop-area "Archives" {:on-click #(-> (:popup @s) js/$ .fadeToggle)})
         (when-not (empty? discard) [:<> {:key "discard"} (draw-card (last discard))])

         [label discard {:opts {:name "Archives"
                                :fn (fn [cursor] (let [total (count cursor)
                                                       face-up (count (filter faceup? cursor))]
                                                   ;; use non-breaking space to keep counts on same line.
                                                   (str face-up "↑ " (- total face-up) "↓")))}}]

         [:div.panel.blue-shade.popup {:ref #(swap! s assoc :popup %)
                                       :class (if (= (:side @game-state) :runner) "opponent" "me")}
          [:div
           [:a {:on-click #(close-popup % (:popup @s) nil false false)} "Close"]
           [:label (let [total (count discard)
                         face-up (count (filter faceup? discard))]
                     (str total " cards, " (- total face-up) " face-down."))]]
          (doall (for [c discard]
            ^{:key (:cid c)}
            [:div (draw-card c)]))]]))))

(defn rfg-view [{:keys [cards name popup]}]
  (let [dom (atom {})]
    (fn [{:keys [cards name popup]}]
      (when-not (empty? cards)
        (let [size (count cards)]
          [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")
                                      :on-click (when popup #(-> (:rfg-popup @dom) js/$ .fadeToggle))}
           (map-indexed (fn [i card]
                          [:div.card-wrapper {:key i
                                              :style {:left (when (> size 1)(* (/ 128 size) i))}}
                           [:div [card-view card]]])
                        cards)
           [label cards {:opts {:name name}}]

           (when popup
             [:div.panel.blue-shade.popup {:ref #(swap! dom assoc :rfg-popup %)
                                           :class "opponent"}
              [:div
               [:a {:on-click #(close-popup % (:rfg-popup @dom) nil false false)} "Close"]
               [:label (str size " card" (when (not= 1 size) "s") ".")]]
              (doall (for [c cards]
                ^{:key (:cid c)}
                [card-view c]))])])))))

(defn play-area-view [{:keys [name player]}]
  (let [side (get-in player [:identity :side])]
    (fn [{:keys [name player]}]
      (let [cards (:play-area player)
            size (count cards)]
        (when-not (empty? cards)
          [:div.panel.blue-shade.rfg {:class (when (> size 2) "squeeze")}
           (map-indexed (fn [i card]
                          [:div.card-wrapper {:key i
                                              :style {:left (when (> size 1) (* (/ 128 size) i))}}
                           (if (this-user? player)
                             [card-view card]
                             [facedown-card side])])
                          cards)
           [label cards {:opts {:name name}}]])))))

(defn scored-view [{:keys [scored]}]
  (let [size (count scored)]
    [:div.panel.blue-shade.scored.squeeze
     (map-indexed (fn [i card]
                    [:div.card-wrapper {:key i
                                        :style {:left (when (> size 1) (* (/ 128 (dec size)) i))}}
                     [:div [card-view card]]])
                  scored)
     [label scored {:opts {:name "Scored Area"}}]]))

(defn controls
  "Create the control buttons for the side displays."
  ([key] (controls key 1 -1))
  ([key increment decrement]
   [:div.controls
    [:button.small {:on-click #(send-command "change" {:key key :delta decrement}) :type "button"} "-"]
    [:button.small {:on-click #(send-command "change" {:key key :delta increment}) :type "button"} "+"]]))

(defmulti stats-view #(get-in % [:identity :side]))

(defmethod stats-view "Runner" [{:keys [user click credit run-credit memory link tag
                                        brain-damage agenda-point tagged hand-size active]}]
  (let [me? (= (:side @game-state) :runner)]
    (fn [{:keys [user click credit run-credit memory link tag
                 brain-damage agenda-point tagged hand-size active]}]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis [avatar user {:opts {:size 22}}] (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" ""))
        (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" "")
                  (when (pos? run-credit)
                    (str " (" run-credit " for run)")))
        (when me? (controls :credit))]
       (let [{:keys [base mod used]} memory
             max-mu (+ base mod)
             unused (- max-mu used)]
         [:div (str unused " of " max-mu " MU unused")
          (when (neg? unused) [:div.warning "!"]) (when me? (controls :memory))])
       [:div (str link " Link Strength")
        (when me? (controls :link))]
       [:div (str agenda-point " Agenda Point"
                  (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str tag " Tag" (if (not= tag 1) "s" ""))
        (when (or (pos? tag) (pos? tagged)) [:div.warning "!"]) (when me? (controls :tag))]
       [:div (str brain-damage " Brain Damage")
        (when me? (controls :brain-damage))]
       (let [{:keys [base mod]} hand-size]
         [:div (str (+ base mod) " Max hand size")
          (when me? (controls :hand-size))])])))

(defmethod stats-view "Corp" [{:keys [user click credit agenda-point bad-publicity has-bad-pub hand-size active]}]
  (let [me? (= (:side @game-state) :corp)]
    (fn [{:keys [user click credit agenda-point bad-publicity has-bad-pub hand-size active]}]
      [:div.panel.blue-shade.stats {:class (when active "active-player")}
       [:h4.ellipsis [avatar user {:opts {:size 22}}] (:username user)]
       [:div (str click " Click" (if (not= click 1) "s" ""))
        (when me? (controls :click))]
       [:div (str credit " Credit" (if (not= credit 1) "s" ""))
        (when me? (controls :credit))]
       [:div (str agenda-point " Agenda Point" (when (not= agenda-point 1) "s"))
        (when me? (controls :agenda-point))]
       [:div (str (+ bad-publicity has-bad-pub) " Bad Publicity")
        (when me? (controls :bad-publicity))]
       (let [{:keys [base mod]} hand-size]
         [:div (str (+ base mod) " Max hand size")
          (when me? (controls :hand-size))])])))

(defn run-arrow []
  [:div.run-arrow [:div]])

(defn server-view [{:keys [server central-view run]} opts]
  (let [content (:content server)
        ices (:ices server)
        run-pos (:position run)
        current-ice (when (and run (pos? run-pos) (<= run-pos (count ices)))
                      (nth ices (dec run-pos)))
        max-hosted (apply max (map #(count (:hosted %)) ices))]
    [:div.server
     [:div.ices {:style {:width (when (pos? max-hosted)
                                  (+ 84 3 (* 42 (dec max-hosted))))}}
      (when-let [run-card (:card (:run-effect run))]
        [:div.run-card [card-img run-card]])
      (for [ice (reverse ices)]
        [:div.ice {:key (:cid ice)
                   :class (when (not-empty (:hosted ice)) "host")}
         (let [flipped (not (:rezzed ice))]
           [card-view ice flipped])
         (when (and current-ice (= (:cid current-ice) (:cid ice)))
           [run-arrow])])
      (when (and run (not current-ice))
        [run-arrow])]
     [:div.content
      (when central-view
        central-view)
      (when (not-empty content)
          (for [card content]
                 (let [is-first (= card (first content))
                       flipped (not (:rezzed card))]
                   [:div.server-card {:key (:cid card)
                                      :class (str (when central-view "central ")
                                                  (when (or central-view
                                                            (and (< 1 (count content)) (not is-first)))
                                                    "shift"))}
                    [card-view card flipped]
                    (when (and (not central-view) is-first)
                      [label content {:opts opts}])])))]]))

(defmulti board-view (fn [player run] (get-in @player [:identity :side])))

(defmethod board-view "Corp" [player run]
  (let [servers (r/cursor player [:servers])]
    (fn [player run]
      (let [rs (:server run)
            server-type (first rs)]
        [:div.corp-board {:class (if (= (:side @game-state) :runner) "opponent" "me")}
         (doall
           (for [server (reverse (get-remotes @servers))
                 :let [num (remote->num (first server))]]
             [server-view {:key num
                           :server (second server)
                           :run (when (= server-type (str "remote" num)) run)}
              {:opts {:name (remote->name (first server))}}]))
         [server-view {:key "hq"
                       :server (:hq @servers)
                       :central-view [identity-view @player]
                       :run (when (= server-type "hq") run)}]
         [server-view {:key "rd"
                       :server (:rd @servers)
                       :central-view [deck-view @player]
                       :run (when (= server-type "rd") run)}]
         [server-view {:key "archives"
                       :server (:archives @servers)
                       :central-view [discard-view @player]
                       :run (when (= server-type "archives") run)}]]))))

(defmethod board-view "Runner" [player run]
  (let [is-me (= (:side @game-state) :runner)]
    (fn [player run]
      (let [centrals [:div.runner-centrals
                      [discard-view @player]
                      [deck-view @player]
                      [identity-view @player]]]
        [:div.runner-board {:class (if is-me "me" "opponent")}
         (when-not is-me centrals)
         (doall (for [zone [:program :hardware :resource :facedown]]
           ^{:key zone}
           [:div
            (doall (for [c (zone (:rig @player))]
              ^{:key (:cid c)}
              [:div.card-wrapper {:class (when (playable? c) "playable")}
               [card-view c]]))]))
         (when is-me centrals)]))))

(defn cond-button [text cond f]
  (if cond
    [:button {:on-click f :key text} text]
    [:button.disabled {:key text} text]))

(defn handle-end-turn []
  (let [me ((:side @game-state) @game-state)
        {:keys [base mod]} (:hand-size me)
        max-size (max (+ base mod) 0)]
    (if (> (count (:hand me)) max-size)
      (toast (str "Discard to " max-size " card" (when (not= 1 max-size) "s")) "warning" nil)
      (send-command "end-turn"))))

(defn runnable-servers
  "List of servers the runner can run on"
  [corp runner]
  (let [servers (keys (:servers corp))
        restricted-servers (keys (get-in runner [:register :cannot-run-on-server]))]
    ;; remove restricted servers from all servers to just return allowed servers
    (remove (set restricted-servers) servers)))

(defn button-pane [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
  (let [s (r/atom {})
        autocomp (r/track (fn [] (get-in @game-state [side :prompt 0 :choices :autocomplete])))]
    (r/create-class
      {:display-name "button-pane"

       :component-did-update
       (fn []
         (when (pos? (count @autocomp))
           (-> "#card-title" js/$ (.autocomplete (clj->js {"source" @autocomp})))))

       :reagent-render
       (fn [{:keys [side active-player run end-turn runner-phase-12 corp-phase-12 corp runner me opponent] :as cursor}]
         [:div.button-pane {:on-mouse-over #(card-preview-mouse-over % zoom-channel)
                            :on-mouse-out  #(card-preview-mouse-out % zoom-channel)}
          (if-let [prompt (first (:prompt me))]
            [:div.panel.blue-shade
             [:h4 (for [item (get-message-parts (:msg prompt))] (create-span item))]
             (if-let [n (get-in prompt [:choices :number])]
               [:div
                [:div.credit-select
                 [:select#credit {:default-value (get-in prompt [:choices :default] 0)}
                  (for [i (range (inc n))]
                    [:option {:value i} i])]]
                [:button {:on-click #(send-command "choice"
                                                   {:choice (-> "#credit" js/$ .val str->int)})}
                 "OK"]]
               (cond
                 ;; choice of number of credits
                 (= (:choices prompt) "credit")
                 [:div
                  (when-let [base (:base prompt)]
                    ;; This is the initial trace prompt
                    (if (nil? (:strength prompt))
                      (if (= "corp" (:player prompt))
                        ;; This is a trace prompt for the corp, show runner link + credits
                        [:div.info "Runner: " (:link runner) [:span {:class "anr-icon link"}]
                         " + " (:credit runner) [:span {:class "anr-icon credit"}]]
                        ;; Trace in which the runner pays first, showing base trace strength and corp credits
                        [:div.info "Trace: " (when (:bonus prompt) (+ base (:bonus prompt)) base)
                         " + " (:credit corp) [:span {:class "anr-icon credit"}]])
                      ;; This is a trace prompt for the responder to the trace, show strength
                      (if (= "corp" (:player prompt))
                        [:div.info "vs Trace: " (:strength prompt)]
                        [:div.info "vs Runner: " (:strength prompt) [:span {:class "anr-icon link"}]])))
                  [:div.credit-select
                   ;; Inform user of base trace / link and any bonuses
                   (when-let [base (:base prompt)]
                     (if (nil? (:strength prompt))
                       (if (= "corp" (:player prompt))
                         (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
                           [:span (str strength " + ")])
                         [:span (:link runner) " " [:span {:class "anr-icon link"}] (str " + " )])
                       (if (= "corp" (:player prompt))
                         [:span (:link runner) " " [:span {:class "anr-icon link"}] (str " + " )]
                         (let [strength (when (:bonus prompt) (+ base (:bonus prompt)) base)]
                           [:span (str strength " + ")]))))
                   [:select#credit (for [i (range (inc (:credit me)))]
                                     [:option {:value i :key i} i])] " credits"]
                  [:button {:on-click #(send-command "choice"
                                                     {:choice (-> "#credit" js/$ .val str->int)})}
                   "OK"]]

                 ;; auto-complete text box
                 (:card-title (:choices prompt))
                 [:div
                  [:div.credit-select
                   [:input#card-title {:placeholder "Enter a card title"
                                       :onKeyUp #(when (= 13 (.-keyCode %))
                                                   (-> "#card-submit" js/$ .click)
                                                   (.stopPropagation %))}]]
                  [:button#card-submit {:on-click #(send-command "choice" {:choice (-> "#card-title" js/$ .val)})}
                   "OK"]]

                  ;; choice of specified counters on card
                 (:counter (:choices prompt))
                 (let [counter-type (keyword (:counter (:choices prompt)))
                       num-counters (get-in prompt [:card :counter counter-type] 0)]
                   [:div
                    [:div.credit-select
                     [:select#credit (for [i (range (inc num-counters))]
                                       [:option {:value i} i])] " credits"]
                    [:button {:on-click #(send-command "choice"
                                                       {:choice (-> "#credit" js/$ .val str->int)})}
                     "OK"]])
                 ;; otherwise choice of all present choices
                 :else
                 (map-indexed (fn [i c]
                   (when (not= c "Hide")
                     (if (string? c)
                       [:button {:key i
                                 :on-click #(send-command "choice" {:choice c})}
                        (for [item (get-message-parts c)]
                          (create-span item))]
                       (let [[title code] (extract-card-info (add-image-codes (:title c)))]
                         [:button {:key (:cid c)
                                   :class (when (:rotated c) :rotated)
                                   :on-click #(send-command "choice" {:card c}) :id code} (:title c)]))))
                                (:choices prompt))))]
            (if run
              (let [rs (:server run)
                    kw (keyword (first rs))
                    server (if-let [n (second rs)]
                             (get-in corp [:servers kw n])
                             (get-in corp [:servers kw]))]
                (if (= side :runner)
                  [:div.panel.blue-shade
                   (when-not (:no-action run) [:h4 "Waiting for Corp's actions"])
                   (if (zero? (:position run))
                     [cond-button "Successful Run" (:no-action run) #(send-command "access")]
                     [cond-button "Continue" (:no-action run) #(send-command "continue")])
                   [cond-button "Jack Out" (not (:cannot-jack-out run))
                                #(send-command "jack-out")]]
                  [:div.panel.blue-shade
                   (when (zero? (:position run))
                     [cond-button "Action before access" (not (:no-action run))
                                  #(send-command "corp-phase-43")])
                   [cond-button "No more action" (not (:no-action run))
                                #(send-command "no-action")]]))
              [:div.panel.blue-shade
               (if (= (keyword active-player) side)
                 (when (and (zero? (:click me)) (not end-turn) (not runner-phase-12) (not corp-phase-12))
                   [:button {:on-click #(handle-end-turn)} "End Turn"])
                 (when end-turn
                   [:button {:on-click #(send-command "start-turn")} "Start Turn"]))
               (when (and (= (keyword active-player) side)
                          (or runner-phase-12 corp-phase-12))
                 [:button {:on-click #(send-command "end-phase-12")}
                  (if (= side :corp) "Mandatory Draw" "Take Clicks")])
               (when (= side :runner)
                 [:div
                  [cond-button "Remove Tag"
                               (and (pos? (:click me))
                                    (>= (:credit me) (- 2 (or (:tag-remove-bonus me) 0)))
                                    (pos? (:tag me)))
                               #(send-command "remove-tag")]
                  [:div.run-button
                   [cond-button "Run" (and (pos? (:click me))
                                           (not (get-in me [:register :cannot-run])))
                                #(-> (:servers @s) js/$ .toggle)]
                    [:div.panel.blue-shade.servers-menu {:ref #(swap! s assoc :servers %)}
                     (map-indexed (fn [i label]
                            [:div {:key i
                                   :on-click #(do (send-command "run" {:server label})
                                                  (-> (:servers @s) js/$ .fadeOut))}
                             label])
                          (zones->sorted-names (runnable-servers corp runner)))]]])
                (when (= side :corp)
                  [cond-button "Purge" (>= (:click me) 3) #(send-command "purge")])
                (when (= side :corp)
                  [cond-button "Trash Resource" (and (pos? (:click me))
                                                     (>= (:credit me) (- 2 (or (:trash-cost-bonus me) 0)))
                                                     (or (pos? (:tagged opponent))
                                                         (pos? (:tag opponent))))
                               #(send-command "trash-resource")])
                [cond-button "Draw" (and (pos? (:click me)) (not-empty (:deck me))) #(send-command "draw")]
                [cond-button "Gain Credit" (pos? (:click me)) #(send-command "credit")]]))])})))

(defn play-sfx
  "Plays a list of sounds one after another."
  [sfx soundbank]
  (when-not (empty? sfx)
    (when-let [sfx-key (keyword (first sfx))]
      (.volume (sfx-key soundbank) (/ (str->int (get-in @app-state [:options :sounds-volume])) 100))
      (.play (sfx-key soundbank)))
    (play-sfx (rest sfx) soundbank)))

(defn update-audio [{:keys [gameid sfx sfx-current-id]} soundbank]
  ;; When it's the first game played with this state or when the sound history comes from different game, we skip the cacophony
  (let [sfx-last-played (:sfx-last-played @sfx-state)]
    (when (and (get-in @app-state [:options :sounds])
               (not (nil? sfx-last-played))
               (= gameid (:gameid sfx-last-played)))
      ;; Skip the SFX from queue with id smaller than the one last played, queue the rest
      (let [sfx-to-play (reduce (fn [sfx-list {:keys [id name]}]
                                  (if (> id (:id sfx-last-played))
                                    (conj sfx-list name)
                                    sfx-list)) [] sfx)]
        (play-sfx sfx-to-play soundbank)))
  ;; Remember the most recent sfx id as last played so we don't repeat it later
  (when sfx-current-id
    (swap! sfx-state assoc :sfx-last-played {:gameid gameid :id sfx-current-id}))))

(def corp-stats
  (let [s #(-> @game-state :stats :corp)]
    [["Clicks Gained" #(-> (s) :gain :click)]
     ["Credits Gained" #(-> (s) :gain :credit)]
     ["Credits Lost" #(-> (s) :lose :credit)]
     ["Credits by Click" #(-> (s) :click :credit)]
     ["Cards Drawn" #(-> (s) :gain :card)]
     ["Cards Drawn by Click" #(-> (s) :click :draw)]
     ["Damage Done" #(-> (s) :damage :all)]
     ["Cards Rezzed" #(-> (s) :cards :rezzed)]]))

(def runner-stats
  (let [s #(-> @game-state :stats :runner)]
    [["Clicks Gained" #(-> (s) :gain :click)]
     ["Credits Gained" #(-> (s) :gain :credit)]
     ["Credits Lost" #(-> (s) :lose :credit)]
     ["Credits by Click" #(-> (s) :click :credit)]
     ["Cards Drawn" #(-> (s) :gain :card)]
     ["Cards Drawn by Click" #(-> (s) :click :draw)]
     ["Tags Gained" #(-> (s) :gain :tag)]
     ["Runs Made" #(-> (s) :runs :started)]
     ["Cards Accessed" #(-> (s) :access :cards)]]))

(defn show-stat
  "Determines statistic counter and if it should be shown"
  [side]
  (when-let [stat-fn (-> side second)]
    (let [stat (stat-fn)]
      (if (pos? stat) stat "-"))))

(defn build-game-stats
  "Builds the end of game statistics div & table"
  []
  (let [stats (map-longest list nil corp-stats runner-stats)]
    [:div
     [:table.win.table
      [:tr.win.th
       [:td.win.th "Corp"] [:td.win.th]
       [:td.win.th "Runner"] [:td.win.th]]
      (for [[corp runner] stats]
        [:tr [:td (first corp)] [:td (show-stat corp)]
         [:td (first runner)] [:td (show-stat runner)]])]]))

(defn build-win-box
  "Builds the end of game pop up game end"
  [game-state]
  [:div.win.centered.blue-shade
   [:div
    (:winning-user @game-state) " (" (-> @game-state :winner capitalize)
    (cond
      (= "Decked" (@game-state :reason capitalize))
      (str ") wins due to the Corp being decked on turn " (:turn @game-state))

      (= "Flatline" (@game-state :reason capitalize))
      (str ") wins by flatline on turn " (:turn @game-state))

      (= "Concede" (@game-state :reason capitalize))
      (str ") wins by concession on turn " (:turn @game-state))

      :else
      (str ") wins by scoring agenda points on turn "  (:turn @game-state)))]
   [:div "Time taken: " (-> @game-state :stats :time :elapsed) " minutes"]
   [:br]
   (build-game-stats)
   [:button.win-right {:on-click #(swap! app-state assoc :win-shown true) :type "button"} "✘"]])

(defn gameboard []
  (let [audio-sfx (fn [name] (list (keyword name)
                                   (new js/Howl (clj->js {:src [(str "/sound/" name ".ogg")
                                                                (str "/sound/" name ".mp3")]}))))
        soundbank (apply hash-map (concat
                          (audio-sfx "agenda-score")
                          (audio-sfx "agenda-steal")
                          (audio-sfx "click-advance")
                          (audio-sfx "click-card")
                          (audio-sfx "click-credit")
                          (audio-sfx "click-run")
                          (audio-sfx "click-remove-tag")
                          (audio-sfx "game-end")
                          (audio-sfx "install-corp")
                          (audio-sfx "install-runner")
                          (audio-sfx "play-instant")
                          (audio-sfx "rez-ice")
                          (audio-sfx "rez-other")
                          (audio-sfx "run-successful")
                          (audio-sfx "run-unsuccessful")
                          (audio-sfx "virus-purge")))
        run (r/cursor game-state [:run])
        side (r/cursor game-state [:side])
        turn (r/cursor game-state [:turn])
        end-turn (r/cursor game-state [:end-turn])
        corp-phase-12 (r/cursor game-state [:corp-phase-12])
        runner-phase-12 (r/cursor game-state [:runner-phase-12])
        corp (r/cursor game-state [:corp])
        runner (r/cursor game-state [:runner])
        active-player (r/cursor game-state [:active-player])
        render-board? (r/track (fn [] (and side corp runner)))
        me (r/track (fn [] (when @@render-board?
                             (assoc ((if (= @side :runner) :runner :corp) @game-state)
                               :active (and (pos? @turn) (= (keyword @active-player) @side))))))
        opponent (r/track (fn [] (when @@render-board?
                                   (assoc ((if (= @side :runner) :corp :runner) @game-state)
                                     :active (and (pos? @turn) (= (keyword @active-player) @side))))))]

  (go (while true
        (let [card (<! zoom-channel)]
          (swap! app-state assoc :zoom card))))

    (r/create-class
      {:display-name "gameboard"

       :component-did-update
       (fn []
         (when (get-in @game-state [@side :prompt 0 :show-discard])
           (-> ".me .discard .popup" js/$ .fadeIn))
         (if (= "select" (get-in @game-state [@side :prompt 0 :prompt-type]))
           (set! (.-cursor (.-style (.-body js/document))) "url('/img/gold_crosshair.png') 12 12, crosshair")
           (set! (.-cursor (.-style (.-body js/document))) "default"))
         (when (= "card-title" (get-in @game-state [@side :prompt 0 :prompt-type]))
           (-> "#card-title" js/$ .focus))
         (doseq [{:keys [msg type options]} (get-in @game-state [@side :toast])]
           (toast msg type options))
         (update-audio {:sfx (:sfx @game-state) :sfx-current-id (:sfx-current-id @game-state)
                        :gameid (:gameid @game-state)} soundbank))

       :reagent-render
       (fn []
         (when @@render-board?
           [:div.gameboard
            (when (and (:winner @game-state) (not (:win-shown @app-state)))
              (build-win-box game-state))

            [:div {:class (:background (:options @app-state))}]

            [:div.rightpane
             [:div.card-zoom
              (when-let [card (:zoom @app-state)]
                [card-zoom card])]
             ;; card implementation info
             (when-let [card (:zoom @app-state)]
               (let [implemented (:implementation card)]
                 (case implemented
                   (:full "full") nil
                   [:div.panel.blue-shade.implementation
                    (case implemented
                      nil [:span.unimplemented "Unimplemented"]
                      [:span.impl-msg implemented])])))

             [log-pane]]

            [:div.centralpane
             [board-view opponent @run]
             [board-view me @run]]

            [:div.leftpane
             [:div.opponent
              [hand-view opponent (get-remotes (:servers @corp)) (= @side :spectator) "opponent"]]

             [:div.inner-leftpane

              [:div.left-inner-leftpane
               [:div
                [stats-view @opponent]
                [scored-view @opponent]
                ]
               [:div
                [scored-view @me]
                [stats-view @me]]]

              [:div.right-inner-leftpane
               [:div
                [rfg-view {:cards (:rfg @opponent) :name "Removed from the game" :popup true}]
                [rfg-view {:cards (:rfg @me) :name "Removed from the game" :popup true}]
                [play-area-view {:player @opponent :name "Temporary Zone"}]
                [play-area-view {:player @me :name "Temporary Zone"}]
                [rfg-view {:cards (:current @opponent) :name "Current" :popup false}]
                [rfg-view {:cards (:current @me) :name "Current" :popup false}]
                ]
               (when-not (= @side :spectator)
                 [button-pane {:side @side :active-player @active-player :run @run :end-turn @end-turn
                               :runner-phase-12 @runner-phase-12 :corp-phase-12 @corp-phase-12
                               :corp @corp :runner @runner :me @me :opponent @opponent}])]]

             [:div.me
              [hand-view me (get-remotes (:servers @corp)) true "me"]]
             ]
            ]))})))

; TODO
; test toast.