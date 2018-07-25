(in-ns 'game.cards.upgrades)

(def card-definition-caprice-nisei
  {"Caprice Nisei"
   {:events {:pass-ice {:req (req (and this-server
                                       (= (:position run) 1))) ; trigger when last ice passed
                        :msg "start a Psi game"
                        :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}
             :run {:req (req (and this-server
                                  (zero? (:position run)))) ; trigger on unprotected server
                   :msg "start a Psi game"
                   :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}}
    :abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}})
