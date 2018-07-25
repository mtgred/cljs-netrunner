(in-ns 'game.cards.icebreakers)

(def card-definition-aumakua
  {"Aumakua"
   {:implementation "Add counters manually for access outside of a run or cards that replace access like Ash"
    ; We would need a :once :per-access key to make this work for Gang Sign etc.
    :abilities [(break-sub 1 1)
                {:label "Add a virus counter"
                 :effect (effect (system-msg "manually adds a virus counter to Aumakua")
                                 (add-counter card :virus 1))}]
    :strength-bonus (req (get-virus-counters state side card))
    :events {:run-ends {:req (req (and (not (or (get-in @state [:run :did-trash])
                                                (get-in @state [:run :did-steal])))
                                       (get-in @state [:run :did-access])))
                        :effect (effect (add-counter card :virus 1))}
             :expose {:effect (effect (add-counter card :virus 1))}
             :counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}})
