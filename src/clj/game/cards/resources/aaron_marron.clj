(in-ns 'game.cards.resources)

(def card-definition-aaron-marron
  {"Aaron Marrón"
   (let [am {:effect (effect (add-counter card :power 2)
                             (system-msg :runner (str "places 2 power counters on Aaron Marrón")))}]
     {:abilities [{:counter-cost [:power 1]
                   :msg "remove 1 tag and draw 1 card"
                   :effect (effect (lose-tags 1) (draw))}]
      :events {:agenda-scored am :agenda-stolen am}})})
