(ns nr.gameboard.state
  (:require [nr.appstate :refer [app-state]]
            [reagent.core :as r]))

(defonce game-state (r/atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defonce replay-side (r/atom :spectator))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(defn get-side [state]
  (if (:replay state)
    @replay-side
    (let [user-id (:_id (:user @app-state))]
      (cond
        (= (get-in state [:runner :user :_id]) user-id) :runner
        (= (get-in state [:corp :user :_id]) user-id) :corp
        :else :spectator))))

(defn not-spectator? []
  (not= :spectator (get-side @game-state)))

(defn check-lock?
  "Check if we can clear client lock based on action-id"
  []
  (let [aid [(:side @game-state) :aid]]
    (when (not= (get-in @game-state aid)
                (get-in @last-state aid))
      (reset! lock false))))

