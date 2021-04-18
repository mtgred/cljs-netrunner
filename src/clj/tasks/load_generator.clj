(ns tasks.load-generator
  (:require [clojure.string :refer [join]]
            [gniazdo.core :as ws]
            [clj-uuid :as uuid]
            [org.httpkit.client :as http]
            [web.core :refer [-main]]
            [web.ws :as game-ws-handler]
            [monger.collection :as mc]
            [clojure.tools.cli :refer [parse-opts]]
            [web.db :refer [db]]
            [web.lobby :refer [all-games]])
  (:import java.net.URLEncoder))

;; This print guarantees a coherent print (i.e. parallel prints will not be interleaved)
(defn safe-println [& more]
  (.write *out* (str (join " " more) "\n")))

(defn login
  [username password]
  (let [options {:form-params {:username username
                          :password password}}
    post-res @(http/post "http://localhost:1042/login" options)]
    (if (or (post-res :error) (= 401 (post-res :status)))
      (println "Failed, exception is " (or (post-res :error) (post-res :status)))
        (let [get-res @(http/get "http://localhost:1042" {:as :text :headers {"Cookie" (str (:set-cookie (post-res :headers)))}})]
          (if (or (get-res :error) (= 401 (get-res :status)))
            (println "Failed, exception is " (or (get-res :error) (get-res :status)))
              {"Origin" "http://localhost:1042" "Cookie" (str (:set-cookie (post-res :headers)) ";" (:set-cookie (get-res :headers))) "X-CSRF-Token" (second (re-find #"data-csrf-token=\"(.*?)\"" (str (get-res :body))))})))))

(defn create-game
  [maxUsers]
  (let [client (ws/client)
        corp-client-id (uuid/to-string (uuid/v1))
        runner-client-id (uuid/to-string (uuid/v1))
        corp-login (login "sample2" "password")
        runner-login (login "sample3" "password")
        corp-decks (mc/find-maps db "decks" {:username "sample2"})
        runner-decks (mc/find-maps db "decks" {:username "sample3"})]
    (.setMaxTextMessageSize (.getPolicy client) (* 1024 1024))
    (.start client)

    (safe-println "Login Corp")
      (ws/connect (str "ws://localhost:1042/ws?client-id=" corp-client-id "&csrf-token=" (URLEncoder/encode (corp-login "X-CSRF-Token")))
                :client client
                :extensions ["permessage-deflate"]
                :on-error #(safe-println "corp error" %)
                :on-connect (fn [n] (safe-println "Corp Connected"))
                :on-close (fn [x y] (safe-println "Corp Disconnected"))
                :headers corp-login)
    (safe-println "Login Runner")
      (ws/connect (str "ws://localhost:1042/ws?client-id=" runner-client-id "&csrf-token=" (URLEncoder/encode (runner-login "X-CSRF-Token")))
                :client client
                :on-error #(safe-println "runner error" %)
                :on-connect (fn [n] (safe-println "Runner Connected"))
                :on-close (fn [x y] (safe-println "Runner Disconnected"))
                :headers runner-login)
    (safe-println "Create lobby")

    ;; Corp create lobby
    (game-ws-handler/-msg-handler {:id :lobby/create :ring-req {:user {:username "sample2"}}
                          :client-id corp-client-id
                          :?data {:title "Performance Game"
                                  :format "standard"
                                  :allow-spectator true
                                  :spectatorhands false
                                  :password ""
                                  :room "casual"
                                  :side "Corp"
                                  :options {}}})

    (let [game-id (first (first @all-games))]
      ;;  Runner join
      (game-ws-handler/-msg-handler {:id :lobby/join :ring-req {:user {:username "sample3"}}
                            :client-id runner-client-id
                            :?data {:gameid game-id
                                    :password ""}})
      ;; Select decks
      (game-ws-handler/-msg-handler {:id :lobby/deck :ring-req {:user {:username "sample2"}}
                            :client-id corp-client-id
                            :?data (str (some #(if (= "Corp" ((% :identity) :side)) (% :_id) false) corp-decks))}) ;; find one deck where :identity :side "Corp", then get the _id
      (game-ws-handler/-msg-handler {:id :lobby/deck :ring-req {:user {:username "sample3"}}
                            :client-id runner-client-id
                            :?data (str (some #(if (= "Runner" ((% :identity) :side)) (% :_id) false) runner-decks))}) ;; find one deck where :identity :side "Runner", then get the _id
      (doall
        (pmap
          (fn [n]
            (let [userClientID (uuid/to-string (uuid/v1))
                  creds (login (str "sample" n) "password")
                  socket (ws/connect (str "ws://localhost:1042/ws?client-id=" userClientID "&csrf-token=" (URLEncoder/encode (creds "X-CSRF-Token")))
                                     :client client
                                     :on-error #(safe-println "spectator error" %)
                                     :on-close #(safe-println 'closed %1 %2)
                                     :headers creds)]
              (game-ws-handler/-msg-handler {:id :lobby/watch :ring-req {:user {:username (str "sample" (+ 4 n))}}
                                    :client-id userClientID
                                    :?data {:gameid game-id
                                            :password ""}})
              socket))
          (range maxUsers)))

      (safe-println "Spectators connected")
      (game-ws-handler/-msg-handler {:id :netrunner/start :ring-req {:user {:username "sample2"}}
                          :client-id corp-client-id})
      (safe-println "Started game"))
      ))

(defn usage
  [options-summary]
  (->> [""
        "Usage: lein load-generator [options]"
        ""
        "Options:"
        options-summary]
       (join \newline)))

(def cli-options
   [["-n" "--num NUM" "Number of spectators to connect to game"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]
    :default 1000]])

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn command
  "This is not really a load test per se, but a development tool to assist debugging performance issues.

  This will start the server and create a single game on your environment between users sample2 (Corp) and sample3 (Runner).
  One thousand spectators will be connected to the game by default, from users sample4-sample1003.

  The load generator must be run AFTER creating sample users."
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or errors
              (not-empty arguments))
        (exit 1 (join \newline (conj errors (usage summary))))
        (do
          (-main "dev")
          (create-game (options :num))))))
