(in-ns 'game.cards.resources)

(def card-definition-akshara-sareen
  {"Akshara Sareen"
   {:in-play [:click 1
              :click-per-turn 1]
    :msg "give each player 1 additional [Click] to spend during their turn"
    :effect (effect (gain :corp :click-per-turn 1))
    :leave-play (effect (lose :corp :click-per-turn 1))}})
