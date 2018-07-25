(in-ns 'game.cards.identities)

(def card-definition-nero-severn-information-broker
  {"Nero Severn: Information Broker"
   {:abilities [{:req (req (has-subtype? current-ice "Sentry"))
                 :once :per-turn
                 :msg "jack out when encountering a Sentry"
                 :effect (effect (jack-out nil))}]}})
