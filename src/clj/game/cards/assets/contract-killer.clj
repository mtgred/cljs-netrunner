(in-ns 'game.core)

(def card-definitions-assets-contract-killer
  {"Contract Killer"
   {:advanceable :always
    :abilities [{:label "Trash a connection" :cost [:click 1] :req (req (>= (:advance-counter card) 2))
                 :choices {:req #(has-subtype? % "Connection")}
                 :msg (msg "trash " (:title target)) :effect (effect (trash card) (trash target))}
                {:cost [:click 1] :req (req (>= (:advance-counter card) 2))
                 :delayed-completion true
                 :msg "do 2 meat damage"
                 :effect (effect (trash card) (damage eid :meat 2 {:card card}))}]}})
