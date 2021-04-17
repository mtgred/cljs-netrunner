(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [org.httpkit.sni-client :as sni-client]
            [web.db :as webdb]
            [tasks.utils :refer [replace-collection]]
            [tasks.images :refer [add-images]]
            [monger.collection :as mc]
            [monger.operators :refer [$exists $inc $currentDate]]
            [throttler.core :refer [throttle-fn]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def ^:const edn-base-url "https://raw.githubusercontent.com/NoahTheDuke/netrunner-data/master/edn/raw_data.edn")
(def ^:const jnet-image-url "https://jinteki.net/img/cards/en/default/stock/")

(defn download-edn-data
  [localpath]
  (if localpath
    ((comp edn/read-string slurp) (str localpath "/edn/raw_data.edn"))
    (let [{:keys [status body error] :as resp} @(http/get edn-base-url)]
      (cond
        error (throw (Exception. (str "Failed to download file " error)))
        (= 200 status) (edn/read-string body)
        :else (throw (Exception. (str "Failed to download file, status " status)))))))

(defn write-to-file
  [filename data]
  (io/make-parents filename)
  (spit filename data))

(defn- card-image-file
  "Returns the path to a card's image as a File"
  [code]
  (io/file "resources" "public" "img" "cards" "en" "default" "stock" (str code ".png")))

(defn- download-card-image
  "Download a single card image from NRDB"
  [{:keys [code title]}]
  (binding [org.httpkit.client/*default-client* sni-client/default-client]
    (let [url (str jnet-image-url code ".png")]
      (println "Downloading: " title "\t\t(" url ")")
      (http/get url {:as :byte-array :timeout 120000 :insecure? true}
                (fn [{:keys [status body error]}]
                  (case status
                    404 (println "No image for card" code title)
                    200 (let [card-path (.getPath (card-image-file code))]
                          (io/make-parents card-path)
                          (with-open [w (io/output-stream card-path)]
                            (.write w body)))
                    (println "Error downloading art for card" code error)))))))

(def download-card-image-throttled
  (throttle-fn download-card-image 5 :second))

(defn- expand-card
  "Make a card stub for all previous versions specified in a card."
  [acc card]
  (reduce #(conj %1 {:title (:title card) :code (:code %2)}) acc (:previous-versions card)))

(defn- generate-previous-card-stubs
  "The cards database only has the latest version of a card. Create stubs for previous versions of a card."
  [cards]
  (let [c (filter #(contains? % :previous-versions) cards)]
    (reduce expand-card `() c)))

;; these are cards with multiple faces, so we can't download them directly
(def ^:const cards-to-skip #{"08012" "09001" "26066" "26120"})

(defn download-card-images
  "Download card images (if necessary) from NRDB"
  [cards]
  (let [img-dir (io/file "resources" "public" "img" "cards" "en" "default" "stock")]
    (io/make-parents img-dir)
    (let [previous-cards (generate-previous-card-stubs cards)
          total-cards (concat cards previous-cards)
          total-cards (remove #(get cards-to-skip (:code %)) total-cards)
          missing-cards (remove #(.exists (card-image-file (:code %))) total-cards)
          total (count total-cards)
          missing (count missing-cards)]
      (if (pos? missing)
        (do
          (println "Have art for" (str (- total missing) "/" total) "cards. Downloading" missing "missing images...")
          (let [futures (doall (map download-card-image-throttled missing-cards))]
            (doseq [resp futures]
              ; wait for all the GETs to complete
              (:status @resp)))
          (println "Finished downloading card art"))
        (println "All" total "card images exist, skipping download")))))

(defn- update-config
  "Store import meta info in the db"
  []
  (mc/update webdb/db "config"
             {:cards-version {$exists true}}
             {$inc {:cards-version 1}
              $currentDate {:last-updated true}}
             {:upsert true}))

(defn fetch-data
  [{:keys [card-images db local db-connection]}]
  (let [edn (dissoc (download-edn-data local) :promos)]
    (doseq [[k data] edn
            :let [filename (str "data/" (name k) ".edn")]]
      (write-to-file filename data)
      (println (str "Wrote data/" filename ".edn to disk")))
    (when db
      (when (not db-connection)
        (webdb/connect))
      (try
        (doseq [[k data] edn
                :let [col (name k)]]
          (replace-collection col data)
          (println (str "Imported " col " into database")))
        (update-config)
        (when card-images
          (download-card-images (:cards edn)))
        (add-images)
        (finally (when (not db-connection) (webdb/disconnect)))))
    (println (count (:cycles edn)) "cycles imported")
    (println (count (:sets edn)) "sets imported")
    (println (count (:mwls edn)) "MWL versions imported")
    (println (count (:cards edn)) "cards imported")))
