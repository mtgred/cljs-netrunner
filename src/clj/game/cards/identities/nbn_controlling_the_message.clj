(in-ns 'game.cards.identities)

(def card-definition-nbn-controlling-the-message
  {"NBN: Controlling the Message"
   (let [cleanup (effect (update! :corp (dissoc card :saw-trash)))]
   {:events {:corp-turn-ends {:effect cleanup}
             :runner-turn-ends {:effect cleanup}
             :runner-trash
             {:async true
              :req (req (and (not (:saw-trash card))
                             (card-is? target :side :corp)
                             (installed? target)))
              :effect (req (show-wait-prompt state :runner "Corp to use NBN: Controlling the Message")
                           (update! state :corp (assoc card :saw-trash true))
                           (continue-ability
                             state :corp
                             {:optional
                              {:prompt "Trace the Runner with NBN: Controlling the Message?"
                               :yes-ability {:trace {:base 4
                                                     :successful
                                                     {:msg "give the Runner 1 tag"
                                                      :async true
                                                      :effect (effect (gain-tags :corp eid 1 {:unpreventable true}))}}}
                               :end-effect (effect (clear-wait-prompt :runner))}}
                             card nil))}}})})
