(in-ns 'game.core)

(def card-definitions-resources-fan-site
  {"Fan Site"
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 0 agenda points"
                             :req (req (installed? card))
                             :effect (effect (as-agenda :runner card 0))}}}})
