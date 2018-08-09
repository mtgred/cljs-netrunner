(in-ns 'game.cards.hardware)

(def card-definition-clone-chip
  {"Clone Chip"
   {:abilities [{:label "Install a program"
                 :prompt "Select a program to install from your heap"
                 :priority true
                 :show-discard true
                 :req (req (and (not (seq (get-in @state [:runner :locked :discard])))
                                (not (install-locked? state side))))
                 :choices {:req #(and (is-type? % "Program")
                                      (= (:zone %) [:discard]))}
                 :effect (req (when (>= (:credit runner) (:cost target))
                                (runner-install state side target)
                                (trash state side card {:cause :ability-cost})
                                (system-msg state side (str "uses " (:title card) " to install " (:title target)))))}]}})