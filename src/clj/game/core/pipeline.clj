(ns game.core.pipeline
  (:require
    [cond-plus.core :refer [cond+]]
    [game.core.eid :refer [make-eid register-effect-completed]]
    [game.core.prompts :refer [show-select]]
    ))

; {
;  :eid {:eid 12345}
;  :continue-fn (fn [new-eid]
;                 (resolve-ability state side new-eid card targets)
;                 )
;  }

(defn make-step
  ([continue-fn] (make-step :paid-ability continue-fn))
  ([phase continue-fn]
   {:phase phase
    :continue-fn continue-fn}))

(comment
  (make-step identity)
  (make-step :basic identity)
  )

(defn queue-step
  [state step]
  (swap! state update-in [:gp :queue] conj step))

(defn get-current-step
  [state]
  (first (get-in @state [:gp :pipeline])))

(defn update-current-step
  [state step]
  (swap! state update-in [:gp :pipeline] #(into [] (cons step (next %)))))

(comment
  (update-current-step (atom {:gp {:pipeline [1 2 3]}}) 5)
  )

(defn drop-current-step
  [state]
  (swap! state update-in [:gp :pipeline] #(into [] (next %))))

(comment
  (drop-current-step (atom {:gp {:pipeline [1 2 3]}}))
  )

(defn continue-gp
  [state]
  (let [gp (:gp @state)
        gp (assoc gp
                  :pipeline (into [] (concat (:queue gp) (:pipeline gp)))
                  :queue [])]
    (swap! state assoc :gp gp)
    (when-let [{:keys [eid continue-fn] :as step} (get-current-step state)]
      (cond+
        [(and eid (get-in @state [:effect-completed (:eid eid)]))]
        [eid
         (drop-current-step state)
         (recur state)]
        [:else
         (let [new-eid (make-eid state)]
           (update-current-step state (assoc step :eid new-eid))
           (register-effect-completed state new-eid (fn [eid] true))
           (continue-fn new-eid)
           (when-not (get-in @state [:effect-completed (:eid new-eid)])
             (drop-current-step state)
             (recur state)))]))))
