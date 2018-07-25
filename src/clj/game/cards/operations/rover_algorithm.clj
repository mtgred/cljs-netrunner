(in-ns 'game.cards.operations)

(def card-definition-rover-algorithm
  {"Rover Algorithm"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "host it as a condition counter on " (card-str state target))
    :effect (effect (host target (assoc card :zone [:discard] :seen true :condition true))
                    (update-ice-strength (get-card state target)))
    :events {:pass-ice {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (add-counter card :power 1))}
             :pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus (get-counters card :power) target))}}}})
