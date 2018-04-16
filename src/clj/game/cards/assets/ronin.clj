(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-ronin
  {"Ronin"
   {:advanceable :always
    :abilities [{:cost [:click 1] :req (req (>= (:advance-counter card) 4))
                 :msg "do 3 net damage"
                 :delayed-completion true
                 :effect (effect (trash card) (damage eid :net 3 {:card card}))}]}})
