(in-ns 'game.cards.operations)

(def card-definition-voter-intimidation
  {"Voter Intimidation"
   {:req (req (seq (:scored runner)))
    :psi {:not-equal {:player :corp
                      :prompt "Select a resource to trash"
                      :choices {:req #(and (installed? %)
                                           (is-type? % "Resource"))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}}}})
