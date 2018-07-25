(in-ns 'game.cards.icebreakers)

(def card-definition-sharpshooter
  {"Sharpshooter"
   (auto-icebreaker ["Destroyer"]
                    {:abilities [{:label "[Trash]: Break any number of Destroyer subroutines"
                                  :msg "break any number of Destroyer subroutines"
                                  :effect (effect (trash card {:cause :ability-cost}))}
                                 (strength-pump 1 2)]})})
