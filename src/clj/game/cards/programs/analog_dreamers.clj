(in-ns 'game.cards.programs)

(def card-definition-analog-dreamers
  {"Analog Dreamers"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect
                           (run :rd
                                {:req (req (= target :rd))
                                 :replace-access
                                 {:prompt "Choose a card to shuffle into R&D"
                                  :choices {:req #(and (not (ice? %))
                                                       (not (rezzed? %))
                                                       (zero? (get-counters % :advancement)))}
                                  :effect (req (move state :corp target :deck)
                                               (shuffle! state :corp :deck)
                                               (swap! state update-in [:runner :prompt] rest)
                                               (handle-end-run state side)) ; remove the replace-access prompt
                                  :msg "shuffle a card into R&D"}}
                                card))}]}})
