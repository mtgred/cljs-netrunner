(in-ns 'game.cards.programs)

(def card-definition-self-modifying-code
  {"Self-modifying Code"
   {:abilities [{:req (req (not (install-locked? state side)))
                 :label "Install program from stack"
                 :effect (req (wait-for (trash state side card {:cause :ability-cost})
                                        (continue-ability
                                          state side
                                          {:cost [:credit 2]
                                           :prompt "Choose a program to install"
                                           :msg (req (if (not= target "No install")
                                                       (str "install " (:title target))
                                                       (str "shuffle their Stack")))
                                           :priority true
                                           :choices (req (cancellable
                                                           (conj (vec (sort-by :title (filter #(is-type? % "Program")
                                                                                              (:deck runner))))
                                                                 "No install")))
                                           :effect (req (trigger-event state side :searched-stack nil)
                                                        (shuffle! state side :deck)
                                                        (when (not= target "No install")
                                                          (runner-install state side target)))}
                                          card nil)))}]}})
