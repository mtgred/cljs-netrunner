(in-ns 'game.cards.operations)

(def card-definition-dedication-ceremony
  {"Dedication Ceremony"
   {:prompt "Select a faceup card"
    :choices {:req #(or (and (card-is? % :side :corp)
                             (:rezzed %))
                        (and (card-is? % :side :runner)
                             (or (installed? %)
                                 (:host %))
                             (not (facedown? %))))}
    :msg (msg "place 3 advancement tokens on " (card-str state target))
    :effect (req (add-prop state :corp target :advance-counter 3 {:placed true})
                 (effect-completed state side eid)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag! state side
                     target :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (get-counters card :advancement) (or (:current-cost card)
                                                                         (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                         true)))))}})
