(in-ns 'game.cards.resources)

(def card-definition-rolodex
  {"Rolodex"
   {:async true
    :msg "look at the top 5 cards of their Stack"
    :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their Stack")
                 (let [from (take 5 (:deck runner))]
                   (if (pos? (count from))
                     (continue-ability
                       state side
                       (reorder-choice :runner :corp from '() (count from) from)
                       card nil)
                     (do (clear-wait-prompt state :corp)
                         (effect-completed state side eid)))))
    :trash-effect {:effect (effect (system-msg :runner
                                               (str "trashes "
                                                    (join ", " (map :title (take 3 (:deck runner))))
                                                    " from their Stack due to Rolodex being trashed"))
                                   (mill :runner 3))}}})
