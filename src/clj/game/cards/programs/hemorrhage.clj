(in-ns 'game.cards.programs)

(def card-definition-hemorrhage
  {"Hemorrhage"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :virus 1))}}
    :abilities [{:counter-cost [:virus 2]
                 :cost [:click 1]
                 :req (req (pos? (count (:hand corp))))
                 :msg "force the Corp to trash 1 card from HQ"
                 :effect (req (show-wait-prompt state :runner "Corp to trash a card from HQ")
                              (resolve-ability
                                state :corp
                                {:prompt "Choose a card to trash"
                                 :choices (req (filter #(= (:side %) "Corp") (:hand corp)))
                                 :effect (effect (trash target)
                                                 (clear-wait-prompt :runner))}
                               card nil))}]}})
