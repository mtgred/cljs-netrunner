(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-cortex-lock
  {"Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                   :msg (msg "do " (:memory runner) " net damage")
                   :effect (effect (damage eid :net (:memory runner) {:card card}))}]}})
