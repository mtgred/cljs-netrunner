(in-ns 'game.cards.upgrades)

(def card-definition-cyberdex-virus-suite
  {"Cyberdex Virus Suite"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (effect (show-wait-prompt :runner "Corp to use Cyberdex Virus Suite")
                             (continue-ability
                               {:optional {:prompt "Purge virus counters with Cyberdex Virus Suite?"
                                           :yes-ability {:msg (msg "purge virus counters")
                                                         :effect (effect (clear-wait-prompt :runner)
                                                                         (purge))}
                                           :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                               card nil))}
    :abilities [{:label "[Trash]: Purge virus counters"
                 :msg "purge virus counters" :effect (effect (trash card) (purge))}]}})
