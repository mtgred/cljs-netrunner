(in-ns 'game.cards.operations)

(def card-definition-patch
  {"Patch"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "give +2 strength to " (card-str state target))
    :effect (effect (host target (assoc card :zone [:discard] :seen true :condition true))
                    (update-ice-strength (get-card state target)))
    :events {:pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus 2 target))}}}})
