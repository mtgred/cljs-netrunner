(in-ns 'game.cards.hardware)

(def card-definition-lemuria-codecracker
  {"Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1]
                 :req (req (some #{:hq} (:successful-run runner-reg)))
                 :choices {:req installed?}
                 :effect (effect (expose eid target))
                 :msg "expose 1 card"}]}})
