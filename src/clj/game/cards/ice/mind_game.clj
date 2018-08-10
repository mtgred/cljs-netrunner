(in-ns 'game.cards.ice)

(def card-definition-mind-game
  {"Mind Game"
   {:subroutines [(do-psi {:label "Redirect the run to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req (remove #{(-> @state :run :server central->name)} servers))
                           :msg (msg "redirect the run to " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                           :server (rest dest)))))})]
    :runner-abilities [{:label "Add an installed card to the bottom of your Stack"
                        :prompt "Choose one of your installed cards"
                        :choices {:req #(and (installed? %)
                                             (= (:side %) "Runner"))}
                        :effect (effect (move target :deck)
                                        (system-msg :runner (str "adds " (:title target) " to the bottom of their Stack")))}]}})
