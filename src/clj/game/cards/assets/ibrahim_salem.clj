(in-ns 'game.cards.assets)

(def card-definition-ibrahim-salem
  {"Ibrahim Salem"
   (let [trash-ability (fn [card-type]
                         {:req (req (seq (filter #(is-type? % card-type) (:hand runner))))
                          :prompt (str "Choose a " card-type " to trash")
                          :choices (req (filter #(is-type? % card-type) (:hand runner)))
                          :effect (effect (trash target))
                          :msg (msg " trash " (:title target) " from the Runner's Grip")})
         choose-ability {:label "Trash 1 card in the Runner's Grip of a named type"
                         :once :per-turn
                         :req (req (seq (:hand runner)))
                         :prompt "Choose a card type"
                         :choices ["Event" "Hardware" "Program" "Resource"]
                         :msg (msg "reveal " (join ", " (map :title (:hand runner))) " and trash a " target)
                         :effect (effect (resolve-ability (trash-ability target) card nil))}]
     {:additional-cost [:forfeit]
      :flags {:corp-phase-12 (constantly true)}
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :abilities [choose-ability]})})
