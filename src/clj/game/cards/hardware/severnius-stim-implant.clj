(in-ns 'game.core)

(def card-hardware-severnius-stim-implant
  {"Severnius Stim Implant"
   {:abilities [{:cost [:click 1]
                 :prompt "Choose a server to run with Severnius Stim Implant" :choices ["HQ" "R&D"]
                 :effect (req (let [n (count (:hand runner))
                                    srv target]
                                (resolve-ability state side
                                  {:prompt "Choose at least 2 cards in your Grip to trash with Severnius Stim Implant"
                                   :choices {:max n :req #(and (= (:side %) "Runner") (in-hand? %))}
                                   :msg (msg "trash " (count targets) " card" (if (not= 1 (count targets)) "s")
                                             " and access " (quot (count targets) 2) " additional cards")
                                   :effect (req (let [bonus (quot (count targets) 2)]
                                                   (trash-cards state side (make-eid state) targets
                                                                {:unpreventable true :suppress-event true})
                                                   (game.core/run state side srv nil card)
                                                   (register-events state side
                                                     {:pre-access
                                                      {:silent (req true)
                                                       :effect (effect (access-bonus bonus))}
                                                      :run-ends {:effect (effect (unregister-events card))}} card)))}
                                 card nil)))}]
    :events {:pre-access nil :run-ends nil}}})
