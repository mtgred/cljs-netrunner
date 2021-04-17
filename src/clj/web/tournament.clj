(ns web.tournament
  (:require [clojure.string :refer [lower-case]]
            [web.db :refer [db find-maps-case-insensitive object-id]]
            [web.lobby :refer [all-games refresh-lobby close-lobby]]
            [web.utils :refer [response]]
            [web.ws :as ws]
            [jinteki.utils :refer [str->int]]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-uuid :as uuid]
            [clojure.string :as str]))

(defn auth [req]
  (response 200 {:message "ok"}))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn download-cobra-data
  [id]
  (let [data (http/get (str "http://cobr.ai/tournaments/" id ".json"))
        {:keys [status body error headers] :as resp} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (and
        (= 200 status)
        (str/includes? (:content-type headers) "application/json"))
      (assoc (parse-response body) :cobra-link id)
      :else (throw (Exception. (str "Failed to download file, status " status))))))

(defn build-players
  [data]
  (into {} (for [player (:players data)]
             [(:id player) player])))

(defn latest-round
  [data]
  (last (:rounds data)))

(defn get-player-name
  [players player]
  (:name (get players (:id player))))

(defn transform-player
  [players player]
  (-> player
      (assoc :name (get-player-name players player)
             :score (:combinedScore player))
      (dissoc :corpScore :runnerScore :combinedScore)))

(defn determine-winner
  [table]
  (let [player1-score (get-in table [:player1 :score])
        player2-score (get-in table [:player2 :score])]
    (cond
      (or (nil? player1-score)
          (nil? player2-score)) :not-finished
      (> player1-score player2-score) :player1
      (< player1-score player2-score) :player2
      :else :tie)))

(defn process-table
  [table players]
  (let [player1 (transform-player players (:player1 table))
        player2 (transform-player players (:player2 table))]
    (when (and (:name player1) (:name player2))
      (as-> table table
        (assoc table :player1 player1 :player2 player2)
        (assoc table :winner (determine-winner table))
        (dissoc table :intentionalDraw :eliminationGame)))))

(defn process-round
  [round players]
  (keep #(process-table % players) round))

(defn process-all-rounds
  [data players]
  (map #(process-round % players) (:rounds data)))

(defn create-tournament-lobby
  [{:keys [tournament-name tournament-format selected-round table
           username1 username2 allow-spectator on-close cobra-link
           save-replay]}]
  (let [gameid (uuid/v4)
        title (str tournament-name ", Round " selected-round
                   ", Table " table ": " username1 " vs " username2)
        query (into [] (for [username [username1 username2]] {:username username}))
        players (->> (find-maps-case-insensitive db "users" {$or query})
                     (map #(select-keys % [:_id :username :emailhash :isadmin :options :stats]))
                     (map #(update % :_id str))
                     (map #(hash-map :user %)))
        players (->> (if (= username1 (get-in (first players) [:user :username]))
                       [(first players) (second players)]
                       [(second players) (first players)])
                     (filter identity)
                     (into []))
        game {:gameid gameid
              :title title
              :room "tournament"
              :format tournament-format
              :tournament-name tournament-name
              :cobra-link cobra-link
              :players players
              :spectators []
              :spectator-count 0
              :messages [{:user "__system__"
                          :text "The game has been created."}]
              :allow-spectator allow-spectator
              :save-replay save-replay
              :spectatorhands false
              :mute-spectators true
              :date (java.util.Date.)
              :last-update (t/now)
              :on-close on-close}]
    (when (= 2 (count players))
      (refresh-lobby gameid game)
      game)))

(defn create-lobbies-for-tournament
  [data selected-round {save-replays :save-replays? :as options}]
  (let [players (build-players data)
        rounds (process-all-rounds data players)
        round (nth rounds selected-round (count rounds))]
    (keep
      (fn [table]
        (let [username1 (get-in table [:player1 :name])
              username2 (get-in table [:player2 :name])
              base {:tournament-name (:name data)
                    :tournament-format "standard"
                    :cobra-link (:cobra-link data)
                    :selected-round (inc selected-round)
                    :table (:table table)
                    :username1 username1
                    :username2 username2
                    :save-replay save-replays
                    :allow-spectator true}]
          (create-tournament-lobby
            (assoc base :on-close #(create-tournament-lobby
                                     (assoc base :username1 username2 :username2 username1))))))
      round)))

(defn load-tournament
  [{{:keys [cobra-link]} :?data
    client-id :client-id}]
  (let [data (download-cobra-data cobra-link)
        player-names (keep :name (:players data))
        query (into [] (for [username player-names] {:username username}))
        db-players (find-maps-case-insensitive db "users" {$or query})
        found-player-names #(seq (filter (fn [e] (= (lower-case %) (lower-case e))) (map :username db-players)))
        missing-players (remove found-player-names player-names)
        players (build-players data)
        rounds (process-all-rounds data players)]
    (ws/broadcast-to! [client-id] :tournament/loaded {:data {:players players
                                                             :missing-players missing-players
                                                             :rounds rounds
                                                             :cobra-link cobra-link
                                                             :tournament-name (:name data)}})))

(defn wrap-with-to-handler
  "Wrap a function in a handler which checks that the user is a tournament organizer."
  [handler]
  (fn [{{user :user} :ring-req reply-fn :?reply-fn :as msg}]
    (if (:tournament-organizer user)
      (do (handler msg)
          (when reply-fn (reply-fn 200)))
      (when reply-fn (reply-fn 403)))))

(defn create-tables
  [{{:keys [cobra-link selected-round save-replays?]} :?data
    client-id :client-id}]
  (let [data (download-cobra-data cobra-link)
        created-rounds (create-lobbies-for-tournament data (str->int selected-round) {:save-replays? save-replays?})]
    (ws/broadcast-to! [client-id] :tournament/created {:data {:created-rounds (count created-rounds)}})))

(defmethod ws/-msg-handler :tournament/create [event]
  ((wrap-with-to-handler create-tables) event))

(defn close-tournament-tables
  [cobra-link]
  (when cobra-link
    (let [tables (for [game (vals @all-games)
                       :when (= cobra-link (:cobra-link game))]
                   {:started false
                    :gameid (:gameid game)})]
      (map #(close-lobby % true) tables))))

(defmethod ws/-msg-handler :tournament/fetch [event]
  ((wrap-with-to-handler load-tournament) event))

(defn- delete-tables
  [{{:keys [cobra-link]} :?data
    client-id :client-id}]
  (let [deleted-rounds (close-tournament-tables cobra-link)]
    (ws/broadcast-to! [client-id] :tournament/deleted {:data {:deleted-rounds (count deleted-rounds)}})))

(defmethod ws/-msg-handler :tournament/delete [event]
  ((wrap-with-to-handler delete-tables) event))
