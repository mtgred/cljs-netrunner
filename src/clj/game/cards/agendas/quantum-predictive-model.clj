(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-quantum-predictive-model
  {"Quantum Predictive Model"
   {:flags {:rd-reveal (req true)}
    :steal-req (req (not tagged))
    :access {:req (req tagged)
             :effect (effect (as-agenda card 1))
             :msg "add it to their score area and gain 1 agenda point"}}})
