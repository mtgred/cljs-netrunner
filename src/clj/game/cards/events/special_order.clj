(in-ns 'game.cards.events)

(def card-definition-special-order
  {"Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))}})
