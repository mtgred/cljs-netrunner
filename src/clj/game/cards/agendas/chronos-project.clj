(in-ns 'game.core)

(def card-definitions-agendas-chronos-project
  {"Chronos Project"
   {:msg "remove all cards in the Runner's Heap from the game"
    :interactive (req true)
    :effect (effect (move-zone :runner :discard :rfg))}})
