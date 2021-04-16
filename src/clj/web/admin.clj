(ns web.admin
  (:require [web.db :refer [db object-id]]
            [web.lobby :refer [all-games]]
            [game.main :as main]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged? updated-existing?]]
            [monger.query :as mq]
            [monger.operators :refer :all]
            [web.config :refer [frontend-version]])
  (:import org.bson.types.ObjectId))

(defn announce-create-handler [{{message :message} :body}]
  (if-not (empty? message)
    (do
      (doseq [{state :state} (vals @all-games)]
        (when state
          (main/handle-announcement state message)))
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing announcement"})))

(defn news-create-handler [{{item :item} :body}]
  (if-not (empty? item)
    (do
      (mc/insert db "news" {:_id (ObjectId.) :item item :date (java.util.Date.)})
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing news item"})))

(defn news-delete-handler [{{id :id} :params}]
  (try
    (if id
      (if (acknowledged? (mc/remove db "news" {:_id (object-id id)}))
        (response 200 {:message "Deleted"})
        (response 403 {:message "Forbidden"}))
      (response 400 {:message "Missing new items id"}))
    (catch Exception ex
      (response 409 {:message "Unknown news item id"}))))

(defn version-handler [req]
  (let [config (mc/find-one-as-map db "config" nil)
        version (:version config "0.0")]
    (response 200 {:message "ok" :version version})))

(defn version-update-handler [{{version :version} :body}]
  (if-not (empty? version)
    (do
      (reset! frontend-version version)
      (mc/update db "config" {} {$set {:version version}})
      (response 200 {:message "ok" :version version}))
    (response 400 {:message "Missing version item"})))

(defn- find-user [criteria]
  (let [data (mq/with-collection db "users"
               (mq/find criteria)
               (mq/fields [:_id :username])
               (mq/sort (array-map :username 1)))]
    (response 200 data)))

(defn- update-user [criteria field value]
  (if (updated-existing? (mc/update db "users" criteria {$set {field value}}))
    (response 200 {:message "ok"})
    (response 404 {:message "Unknown user"})))

(defn mods-handler [req]
  (find-user {:ismoderator true}))

(defn mods-update-handler [{{username :username} :body}]
  (if-not (empty? username)
    (update-user {:username username} :ismoderator true)
    (response 400 {:message "Missing username"})))

(defn mods-delete-handler [{{id :id} :params}]
  (if id
    (update-user {:_id (object-id id)} :ismoderator false)
    (response 400 {:message "Missing id"})))

(defn tos-handler [req]
  (find-user {:tournament-organizer true}))

(defn tos-update-handler [{{username :username} :body}]
  (if-not (empty? username)
    (update-user {:username username} :tournament-organizer true)
    (response 400 {:message "Missing username"})))

(defn tos-delete-handler [{{id :id} :params}]
  (if id
    (update-user {:_id (object-id id)} :tournament-organizer false)
    (response 400 {:message "Missing id"})))

(defn specials-handler [req]
  (find-user {:special {$exists true}}))

(defn specials-update-handler [{{username :username} :body}]
  (if-not (empty? username)
    (update-user {:username username} :special true)
    (response 400 {:message "Missing username"})))

(defn specials-delete-handler [{{id :id} :params}]
  (if id
    (do
      (if (updated-existing? (mc/update db "users" {:_id (object-id id)} {$unset {:special false}}))
        (response 200 {:message "Removed"})
        (response 404 {:message "Unknown user"})))
    (response 400 {:message "Missing id"})))

(defn features-handler [req]
  (let [config (mc/find-one-as-map db "config" nil)
        features (:features config {})]
    (response 200 {:message "ok" :features features})))

(defn features-update-handler [{{version :version} :body}]
  (if-not (empty? version)
    (do
      (reset! frontend-version version)
      (mc/update db "config" {} {$set {:version version}})
      (response 200 {:message "ok" :version version}))
    (response 400 {:message "Missing version item"})))
