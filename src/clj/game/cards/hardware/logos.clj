(in-ns 'game.cards.hardware)

(def card-definition-logos
  {"Logos"
   {:in-play [:memory 1 :hand-size 1]
    :events {:agenda-scored
             {:player :runner :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
              :choices (req (cancellable (:deck runner)))
              :effect (effect (trigger-event :searched-stack nil)
                              (shuffle! :deck)
                              (move target :hand))}}}})
