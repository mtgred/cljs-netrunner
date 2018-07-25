(in-ns 'game.cards.identities)

(def card-definition-hayley-kaplan-universal-scholar
  {"Hayley Kaplan: Universal Scholar"
   {:events {:runner-install
             {:silent (req (not (and (first-event? state side :runner-install)
                                     (some #(is-type? % (:type target)) (:hand runner)))))
              :req (req (and (first-event? state side :runner-install)
                             (some #(is-type? % (:type target)) (:hand runner))))
              :once :per-turn
              :async true
              :effect
              (req (let [itarget target
                         type (:type itarget)]
                     (continue-ability
                       state side
                       {:optional {:prompt (msg "Install another " type " from your Grip?")
                                   :yes-ability
                                   {:prompt (msg "Select another " type " to install from your Grip")
                                    :choices {:req #(and (is-type? % type)
                                                         (in-hand? %))}
                                    :msg (msg "install " (:title target))
                                    :effect (effect (runner-install eid target nil))}}}
                       card nil)))}}}})
