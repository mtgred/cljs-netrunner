(in-ns 'game.cards.assets)

(def card-definition-mca-austerity-policy
  {"MCA Austerity Policy"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :msg "to force the Runner to lose a [Click] next turn and place a power counter on itself"
                 :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                              (add-counter state side card :power 1))}
                {:cost [:click 1]
                 :counter-cost [:power 3]
                 :msg "gain 4 [Click] and trash itself"
                 :effect (effect (trash card {:cause :ability-cost
                                              :unpreventable true})
                                 (gain :click 4))}]}})
