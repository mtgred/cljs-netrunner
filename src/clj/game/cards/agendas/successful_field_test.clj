(in-ns 'game.cards.agendas)

(def card-definition-successful-field-test
  {"Successful Field Test"
   (letfn [(sft [n max] {:prompt "Select a card in HQ to install with Successful Field Test"
                         :priority -1
                         :async true
                         :choices {:req #(and (= (:side %) "Corp")
                                              (not (is-type? % "Operation"))
                                              (in-hand? %))}
                         :effect (req (wait-for
                                        (corp-install state side target nil {:no-install-cost true})
                                        (if (< n max)
                                          (continue-ability state side (sft (inc n) max) card nil)
                                          (effect-completed state side eid))))})]
     {:async true
      :msg "install cards from HQ, ignoring all costs"
      :effect (req (let [max (count (filter #(not (is-type? % "Operation")) (:hand corp)))]
                     (continue-ability state side (sft 1 max) card nil)))})})
