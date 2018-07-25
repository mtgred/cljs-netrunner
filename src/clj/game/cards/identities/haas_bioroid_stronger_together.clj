(in-ns 'game.cards.identities)

(def card-definition-haas-bioroid-stronger-together
  {"Haas-Bioroid: Stronger Together"
   {:events {:pre-ice-strength {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
                                :effect (effect (ice-strength-bonus 1 target))}}
    :leave-play (effect (update-all-ice))
    :effect (effect (update-all-ice))}})
