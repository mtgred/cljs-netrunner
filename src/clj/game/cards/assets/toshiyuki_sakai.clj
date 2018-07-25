(in-ns 'game.cards.assets)

(def card-definition-toshiyuki-sakai
  {"Toshiyuki Sakai"
   (advance-ambush 0
    {:effect (effect (resolve-ability
                       {:prompt "Select an asset or agenda in HQ"
                        :choices {:req #(and (or (is-type? % "Agenda")
                                                 (is-type? % "Asset"))
                                             (in-hand? %))}
                        :msg "swap it for an asset or agenda from HQ"
                        :effect (req (let [tidx (ice-index state card)
                                           srvcont (get-in @state (cons :corp (:zone card)))
                                           c (get-counters (get-card state card) :advancement)
                                           newcard (assoc target :zone (:zone card) :advance-counter c)
                                           newcont (apply conj (subvec srvcont 0 tidx) newcard (subvec srvcont tidx))]
                                       (resolve-ability state side
                                         {:effect (req (swap! state assoc-in (cons :corp (:zone card)) newcont)
                                                       (swap! state update-in [:corp :hand]
                                                         (fn [coll] (remove-once #(= (:cid %) (:cid newcard)) coll)))
                                                       (trigger-event state side :corp-install newcard)
                                                       (move state side card :hand))} card nil)
                                       (resolve-prompt state :runner {:choice "No action"})
                                       ; gets rid of prompt to trash Toshiyuki since it's back in HQ now
                                       (resolve-ability state :runner
                                         {:optional
                                          {:player :runner
                                           :priority true
                                           :prompt "Access the newly installed card?"
                                           :yes-ability {:effect (effect (access-card newcard))}}}
                                         card nil)))}
                      card nil))}
    "Swap Toshiyuki Sakai with an agenda or asset from HQ?")})
