(in-ns 'game.cards.resources)

(def card-definition-mr-li
  {"Mr. Li"
   {:abilities [{:cost [:click 1]
                 :label "Draw cards"
                 :msg (msg "draw 2 cards")
                 :effect (req (draw state side 2)
                              (let [drawn (get-in @state [:runner :register :most-recent-drawn])]
                                (resolve-ability
                                  state side
                                  {:prompt "Select 1 card to add to the bottom of the Stack"
                                   :choices {:req #(and (in-hand? %)
                                                        (some (fn [c] (= (:cid c) (:cid %))) drawn))}
                                   :msg (msg "add 1 card to the bottom of the Stack")
                                   :effect (req (move state side target :deck))}
                                  card nil)))}]}})
