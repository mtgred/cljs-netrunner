(in-ns 'game.cards.operations)

(def card-definition-transparency-initiative
  {"Transparency Initiative"
   {:choices {:req #(and (is-type? % "Agenda")
                         (installed? %)
                         (not (faceup? %)))}
    :effect (effect (update! (assoc target :seen true :rezzed true
                                           :subtype (combine-subtypes false (:subtype target) "Public")))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true))
                    (register-events
                      {:advance {:req (req (= (:hosted card) (:hosted target)))
                                 :effect (effect (gain-credits 1)
                                                 (system-msg
                                                   (str "uses Transparency Initiative to gain 1 [Credit]")))}}
                      target))}})
