(in-ns 'game.cards.events)

(def card-definition-deep-data-mining
  {"Deep Data Mining"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus (max 0 (min 4 (available-mu state)))))}
             :run-ends {:effect (effect (unregister-events card))}}}})
