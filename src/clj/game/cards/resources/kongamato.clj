(in-ns 'game.core)

(def card-definitions-resources-kongamato
  {"Kongamato"
   {:abilities [{:label "[Trash]: Break the first subroutine on the encountered piece of ice"
                 :req (req (and (:run @state) (rezzed? current-ice)))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (system-msg :runner
                                             (str "trashes Kongamato to break the first subroutine on "
                                                  (:title current-ice))))}]}})
