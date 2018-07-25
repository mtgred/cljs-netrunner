(in-ns 'game.cards.events)

(def card-definition-encore
  {"Encore"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (req (swap! state update-in [:runner :extra-turns] (fnil inc 0))
                 (move state side (first (:play-area runner)) :rfg))
    :msg "take an additional turn after this one"}})
