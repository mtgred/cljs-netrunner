(in-ns 'game.cards.operations)

(def card-definition-interns
  {"Interns"
   {:prompt "Select a card to install from Archives or HQ"
    :show-discard true
    :not-distinct true
    :choices {:req #(and (not (is-type? % "Operation"))
                         (= (:side %) "Corp")
                         (#{[:hand] [:discard]} (:zone %)))}
    :effect (effect (corp-install target nil {:no-install-cost true}))
    :msg (msg (corp-install-msg target))}})
