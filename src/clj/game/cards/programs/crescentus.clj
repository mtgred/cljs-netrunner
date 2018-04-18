(in-ns 'game.core)

(def card-definitions-programs-crescentus
  {"Crescentus"
   {:implementation "Does not check that all subroutines were broken"
    :abilities [{:req (req (rezzed? current-ice))
                 :msg (msg "derez " (:title current-ice))
                 :effect (effect (trash card {:cause :ability-cost}) (derez current-ice))}]}})
