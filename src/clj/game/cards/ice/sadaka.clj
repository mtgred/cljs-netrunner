(in-ns 'game.cards.ice)

(def card-definition-sadaka
  {"Sadaka"
   (let [maybe-draw-effect
         {:async true
          :effect (req (show-wait-prompt state :runner "Corp to decide on Sadaka card draw action")
                       (continue-ability
                         state side
                         {:optional
                          {:player :corp
                           :prompt "Draw 1 card?"
                           :yes-ability
                           {:async true
                            :effect (effect (clear-wait-prompt :runner)
                                            (draw eid 1 nil))
                            :msg "draw 1 card"}
                           :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                        (effect-completed eid))}}}
                         card nil))}]
     {:subroutines [{:label "Look at the top 3 cards of R&D"
                     :req (req (not-empty (:deck corp)))
                     :async true
                     :effect (req (let [top-cards (take 3 (:deck corp))
                                        top-names (map :title top-cards)]
                                    (show-wait-prompt state :runner "Corp to decide on Sadaka R&D card actions")
                                    (continue-ability
                                      state side
                                      {:prompt (str "Top 3 cards of R&D: " (join ", " top-names))
                                       :choices ["Arrange cards" "Shuffle R&D"]
                                       :async true
                                       :effect
                                       (req (if (= target "Arrange cards")
                                              (wait-for
                                                (resolve-ability state side (reorder-choice :corp top-cards) card nil)
                                                (system-msg state :corp (str "rearranges the top "
                                                                             (quantify (count top-cards) "card")
                                                                             " of R&D"))
                                                (clear-wait-prompt state :runner)
                                                (continue-ability state side maybe-draw-effect card nil))
                                              (do
                                                (shuffle! state :corp :deck)
                                                (system-msg state :corp (str "shuffles R&D"))
                                                (clear-wait-prompt state :runner)
                                                (continue-ability state side maybe-draw-effect card nil))))}
                                      card nil)))}
                    {:label "Trash 1 card in HQ"
                     :async true
                     :effect
                     (req (show-wait-prompt state :runner "Corp to select cards to trash with Sadaka")
                          (wait-for
                            (resolve-ability
                              state side
                              {:prompt "Choose a card in HQ to trash"
                               :choices (req (cancellable (:hand corp) :sorted))
                               :async true
                               :cancel-effect (effect (system-msg "chooses not to trash a card from HQ")
                                                      (effect-completed eid))
                               :effect (req (wait-for
                                              (trash state :corp (make-eid state) target nil)
                                              (do
                                                (system-msg state :corp "trashes a card from HQ")
                                                (wait-for
                                                  (resolve-ability state side trash-resource-sub card nil)
                                                  (effect-completed state side eid)))))}
                              card nil)
                            (system-msg state :corp "trashes Sadaka")
                            (clear-wait-prompt state :runner)
                            (when current-ice
                              (no-action state side nil)
                              (continue state side nil))
                            (trash state :corp eid card nil)))}]})})
