(in-ns 'game.cards.operations)

(def card-definition-bioroid-efficiency-research
  {"Bioroid Efficiency Research"
   {:implementation "Derez is manual"
    :choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (installed? %)
                         (not (rezzed? %)))}
    :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
    :effect (effect (rez target {:ignore-cost :all-costs})
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}})
