(in-ns 'game.core)

(declare can-host?)

(def card-programs-expert-schedule-analyzer
  {"Expert Schedule Analyzer"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on HQ"
                 :makes-run true
                 :effect (effect (run :hq {:req (req (= target :hq))
                                           :replace-access
                                           {:msg (msg "reveal cards in HQ: "
                                                      (join ", " (map :title (:hand corp))))}} card))}]}})
