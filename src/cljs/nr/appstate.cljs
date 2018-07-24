(ns nr.appstate
  (:require [jinteki.utils :refer [str->int]]
            [reagent.core :as r]))

(defn- get-local-value
  "Read the value of the given key from localStorage. Return the default-value if no matching key found"
  [k default-value]
  (let [rtn (js->clj (.getItem js/localStorage k))]
    (if (nil? rtn) default-value rtn)))

(def app-state
  (r/atom {:active-page "/"
           :user (js->clj js/user :keywordize-keys true)
           :options (merge {:background "lobby-bg"
                            :show-alt-art true
                            :deckstats "always"
                            :gamestats "always"
                            :sounds (= (get-local-value "sounds" "true") "true")
                            :lobby-sounds (= (get-local-value "lobby_sounds" "true") "true")
                            :sounds-volume (str->int (get-local-value "sounds_volume" "100"))}
                           (:options (js->clj js/user :keywordize-keys true)))

           :cards-loaded false
           :sets [] :mwl [] :cycles []
           :decks [] :decks-loaded false
           :stats (:stats (js->clj js/user :keywordize-keys true))
           :games [] :gameid nil :messages []
           :channels {:general [] :america [] :europe [] :asia-pacific [] :united-kingdom [] :français []
                      :español [] :italia [] :português [] :sverige [] :stimhack-league []}
           }))
