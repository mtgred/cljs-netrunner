(in-ns 'game.cards.assets)

(def card-definition-bioroid-work-crew
  {"Bioroid Work Crew"
   {:implementation "Timing restriction of ability use not enforced"
    :abilities [{:label "[Trash]: Install 1 card, paying all costs"
                 :req (req (= (:active-player @state) :corp))
                 :prompt "Select a card in HQ to install"
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (corp-install target nil))
                 :msg (msg (corp-install-msg target))}]}})
