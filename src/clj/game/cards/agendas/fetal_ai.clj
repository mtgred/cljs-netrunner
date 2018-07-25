(in-ns 'game.cards.agendas)

(def card-definition-fetal-ai
  {"Fetal AI"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :req (req (not= (first (:zone card)) :discard)) :msg "do 2 net damage"
             :effect (effect (damage eid :net 2 {:card card}))}
    :steal-cost-bonus (req [:credit 2])}})
