(in-ns 'game.cards.agendas)

(def card-definition-clone-retirement
  {"Clone Retirement"
   {:msg "remove 1 bad publicity" :effect (effect (lose :bad-publicity 1))
    :silent (req true)
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain-bad-publicity :corp 1))}}})
