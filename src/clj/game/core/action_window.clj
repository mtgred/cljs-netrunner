(ns game.core.action-window
  (:require
    [game.core.board :refer [all-active]]
    [game.core.eid :refer [effect-completed make-eid register-effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.pipeline :refer [continue-gp make-step queue-step]]
    [game.macros :refer [continue-ability effect req]]
    [jinteki.utils :refer [add-cost-to-label]]
    ))

(defn generate-action-list
  [state side]
  (->> (all-active state side)
       (concat [(get-in @state [side :basic-action-card])])
       (reduce (fn [acc card]
                 (if-let [abilities (:abilities card)]
                   (->> abilities
                        (filter :action)
                        (map #(do {:title (str (:title card) " - " (add-cost-to-label %))
                                   :ability %
                                   :card card}))
                        (apply conj acc))
                   acc))
               [])))

(defn make-action-window
  [state]
  (let [active-player (:active-player @state)
        clicks-left (get-in @state [active-player :click])]
    (if (and (pos? clicks-left)
             (not (get-in @state [active-player :register :terminal])))
      (let [step (make-step
                   :action-window
                   (fn [eid]
                     (let [new-eid (make-eid state eid)]
                       (register-effect-completed
                         state new-eid (fn [_]
                                         (effect-completed state nil eid)
                                         (make-action-window state)))
                       (resolve-ability
                         state active-player
                         new-eid
                         {:prompt (str "You have " clicks-left " [Click] left. Select an action.")
                          :choices (generate-action-list state active-player)
                          :async true
                          :effect (effect (continue-ability (:ability context) (:card context) nil))
                          }
                         nil nil))))]
        (queue-step state step)
        (continue-gp state)))))
