(in-ns 'game.cards.ice)

(def card-definition-howler
  {"Howler"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
     {:subroutines
      [{:label "Install a piece of Bioroid ICE from HQ or Archives"
        :prompt "Install ICE from HQ or Archives?"
        :choices ["HQ" "Archives"]
        :effect (req (let [fr target]
                       (resolve-ability state side
                                        {:prompt "Choose a Bioroid ICE to install"
                                         :choices (req (filter #(and (ice? %)
                                                                     (has-subtype? % "Bioroid"))
                                                               ((if (= fr "HQ") :hand :discard) corp)))
                                         :effect (req (let [newice (assoc target :zone (:zone card) :rezzed true)
                                                            hndx (ice-index state card)
                                                            ices (get-in @state (cons :corp (:zone card)))
                                                            newices (apply conj (subvec ices 0 hndx) newice (subvec ices hndx))]
                                                        (swap! state assoc-in (cons :corp (:zone card)) newices)
                                                        (swap! state update-in (cons :corp (:zone target))
                                                               (fn [coll] (remove-once #(= (:cid %) (:cid target)) coll)))
                                                        (update! state side (assoc card :howler-target newice))
                                                        (card-init state side newice {:resolve-effect false
                                                                                      :init-data true})
                                                        (trigger-event state side :corp-install newice)))} card nil)))}]
      :events {:run-ends {:req (req (:howler-target card))
                          :effect (effect (trash card {:cause :self-trash})
                                          (derez (get-card state (:howler-target card))))}}})})
