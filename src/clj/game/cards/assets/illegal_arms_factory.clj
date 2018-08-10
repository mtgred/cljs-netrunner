(in-ns 'game.cards.assets)

(def card-definition-illegal-arms-factory
  {"Illegal Arms Factory"
   (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                  :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                  :once :per-turn
                  :async true
                  :req (req (:corp-phase-12 @state))
                  :effect (effect (gain-credits 1)
                                  (draw eid 1 nil))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]
      :trash-effect {:req (req (= :servers (first (:previous-zone card)))
                               (= side :runner))
                     :effect (effect (gain-bad-publicity :corp 1)
                                     (system-msg :corp (str "takes 1 bad publicity from Illegal Arms Factory")))}})})
