(ns nr.chat
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :as s]
            [nr.ajax :refer [GET PUT]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [avatar authenticated] :as auth]
            [nr.gameboard :refer [card-preview-mouse-over card-preview-mouse-out get-message-parts create-span card-zoom] :as gameboard]
            [nr.utils :refer [toastr-options]]
            [nr.ws :as ws]
            [reagent.core :as r]))

(declare fetch-messages)

(defonce chat-state (atom {}))

(def chat-channel (chan))

(ws/register-ws-handler!
  :chat/message
  (partial put! chat-channel))

(defn current-block-list
  []
  (if-let [curr (get-in @app-state [:options :blocked-users] nil)]
    curr
    []))

(defn filter-blocked-messages
  [messages]
  (filter #(= -1 (.indexOf (current-block-list) (:username %))) messages))

(defn update-message-channel
  [channel messages]
  (swap! app-state assoc-in [:channels channel] (filter-blocked-messages messages)))

(go (while true
      (let [msg (<! chat-channel)
            ch (keyword (:channel msg))
            messages (get-in @app-state [:channels ch])]
        (update-message-channel ch (reverse (conj (reverse messages) msg))))))

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

(defn- post-response [blocked-user response]
  (if (= 200 (:status response))
    (non-game-toast (str "Blocked user " blocked-user ". Refresh browser to update.") "success" nil)
    (non-game-toast "Failed to block user" "error" nil)))

(defn block-user
  [blocked-user]
  (authenticated
    (fn [user]
      (let [my-user-name (:username user)
            current-blocked-list (current-block-list)]
        (when (and (not (s/blank? blocked-user))
                   (not= my-user-name blocked-user)
                   (= -1 (.indexOf current-blocked-list blocked-user)))
          (let [new-block-list (conj current-blocked-list blocked-user)]
            (swap! app-state assoc-in [:options :blocked-users] new-block-list)
            (nr.account/post-options "/profile" (partial post-response blocked-user))))))))

(defn send-msg [s channel]
  (authenticated
    (fn [user]
      (let [input (:msg-input @chat-state)
            text (:msg @s)]
        (when-not (empty? text)
          (ws/ws-send! [:chat/say {:channel   (name channel)
                                   :msg       text
                                   :username  (:username user)
                                   :emailhash (:emailhash user)}])
          (let [msg-list (:message-list @chat-state)]
            (set! (.-scrollTop msg-list) (+ (.-scrollHeight msg-list) 500)))
          (swap! s assoc :msg "")
          (.focus input))))))

(defn msg-input-view [channel]
  (let [s (r/atom {})]
    (fn [channel]
      [:form.msg-box {:on-submit #(do (.preventDefault %)
                                      (send-msg s channel))}
       [:input {:type "text" :ref #(swap! chat-state assoc :msg-input %)
                :placeholder "Say something...." :accessKey "l" :value (:msg @s)
                :on-change #(swap! s assoc :msg (-> % .-target .-value))}]
       [:button "Send"]])))

(defn channel-view [{:keys [channel active-channel]} s]
  [:div.block-link {:class (if (= active-channel channel) "active" "")
                    :on-click #(do (swap! s assoc :scrolling false)
                                   (swap! s assoc :channel
                                          (keyword (s/replace (-> % .-target .-innerHTML) #"#" ""))))}
   (str "#" (name channel))])

(defn- hide-block-menu []
  (-> (:msg-buttons @chat-state) js/$ .hide))

(defn message-view [message s]
  (let [user (:user @app-state)
        my-msg (= (:username message) (:username user))]
    (fn [message s]
      [:div.message
      [avatar message {:opts {:size 38}}]
      [:div.content
       [:div.name-menu
        [:span.username
         {:on-click #(-> (:msg-buttons @chat-state) js/$ .toggle)
          :class (if my-msg "" "clickable")}
         (:username message)]
        (when user
          (when (not my-msg)
            [:div.panel.blue-shade.block-menu
             {:ref #(swap! chat-state assoc :msg-buttons %)}
             [:div {:on-click #(do
                                 (block-user (:username message))
                                 (hide-block-menu))} "Block User"]
             [:div {:on-click #(hide-block-menu)} "Cancel"]]))
        [:span.date (-> (:date message) js/Date. js/moment (.format "dddd MMM Do - HH:mm"))]]
       [:div
        {:on-mouse-over #(card-preview-mouse-over % (:zoom-ch @s))
         :on-mouse-out  #(card-preview-mouse-out % (:zoom-ch @s))}
        (let [parts (get-message-parts (:msg message))]
          (doall (map-indexed
            (fn [i item]
              (create-span item)) parts)))]]])))

(defn fetch-messages [s]
  (let [channel (:channel @s)
        messages (get-in @app-state [:channels channel])]
    (when (empty? messages)
      (go (let [x (<! (GET (str "/messages/" (name channel))))
                data (:json x)]
            (update-message-channel channel data))))))

(defn chat []
  (let [s (r/atom {:channel :general
                   :zoom false
                   :zoom-ch (chan)
                   :scrolling false})
        old (atom {:prev-msg-count 0}) ; old is not a r/atom so we don't render when this is updated
        cards-loaded (r/cursor app-state [:cards-loaded])
        user (r/cursor app-state [:user])]

    (r/create-class
      {:display-name "chat"

       :component-will-mount
       (fn []
         (fetch-messages s)
         (go (while true
               (let [card (<! (:zoom-ch @s))]
                 (swap! s assoc :zoom card)))))

       :component-did-update
       (fn []
         (fetch-messages s)
         (when-let [msg-list (:message-list @chat-state)]
           (let [curr-channel (:channel @s)
                 prev-channel (:prev-channel @old)
                 curr-msg-count (count (get-in @app-state [:channels curr-channel]))
                 prev-msg-count (:prev-msg-count @old)
                 curr-page (:active-page @app-state)
                 prev-page (:prev-page @old)
                 is-scrolled (:scrolling @s)]
             (when (or (and (zero? (.-scrollTop msg-list))
                            (not is-scrolled))
                       (not= curr-page prev-page)
                       (not= curr-channel prev-channel)
                       (and (not= curr-msg-count prev-msg-count)
                            (not is-scrolled)))
               (set! (.-scrollTop msg-list) (.-scrollHeight msg-list))
               ; use an atom instead of prev-props as r/current-component is only valid in component functions
               (swap! old assoc :prev-page curr-page)
               (swap! old assoc :prev-channel curr-channel)
               (swap! old assoc :prev-msg-count curr-msg-count)))))

       :reagent-render
       (fn []
         [:div.chat-app
          [:div.blue-shade.panel.channel-list
           [:h4 "Channels"]
           (doall
             (for
               [ch [:general :america :europe :asia-pacific :united-kingdom :français :español :italia :polska
                    :português :sverige :stimhack-league :русский]]
               ^{:key ch}
               [channel-view {:channel ch :active-channel (:channel @s)} s]))]
          [:div.chat-container
           [:div.chat-card-zoom
            (when-let [card (:zoom @s)]
              [card-zoom card])]
           [:div.chat-box
            [:div.blue-shade.panel.message-list {:ref #(swap! chat-state assoc :message-list %)
                                                 :on-scroll #(let [currElt (.-currentTarget %)
                                                                   scroll-top (.-scrollTop currElt)
                                                                   scroll-height (.-scrollHeight currElt)
                                                                   client-height (.-clientHeight currElt)
                                                                   scrolling (< (+ scroll-top client-height) scroll-height)]
                                                               (swap! s assoc :scrolling scrolling))}
             (if (not cards-loaded)
               [:h4 "Loading cards..."]
               (let [message-list (get-in @app-state [:channels (:channel @s)])]
                 (doall (map-indexed
                   (fn [i message]
                     [:div {:key i}
                      [message-view message s]]) message-list))))]
            (when @user
              [:div
               [msg-input-view (:channel @s)]])]]])})))

; TODO hyperlink card texr unreliable  -- or rror at start