(in-ns 'game.cards.ice)

(def card-definition-marker
  {"Marker"
   {:subroutines [{:label "Give the next ICE encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give the next ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}})
