(in-ns 'game.core)

(def card-definitions-events-knifed
  {"Knifed"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})
