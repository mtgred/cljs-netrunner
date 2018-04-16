(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-executive-retreat
  {"Executive Retreat"
   {:effect (effect (add-counter card :agenda 1)
                    (shuffle-into-deck :hand))
    :interactive (req true)
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :msg "draw 5 cards"
                 :effect (effect (draw 5))}]}})
