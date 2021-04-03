(ns game.core.action-window-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.action-window :as sut]
            [clojure.test :refer :all]))

(deftest action-window-test
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Melange Mining Corp."]
                      :credits 10}})
    (core/gain state :corp :click 5)
    (play-from-hand state :corp "Melange Mining Corp." "New remote")
    (rez state :corp (get-content state :remote1 0))
    (sut/make-action-window state)
    (click-prompt state :corp "Melange Mining Corp. - [Click][Click][Click]: Gain 7 [Credits]")
    (click-prompt state :corp "Melange Mining Corp. - [Click][Click][Click]: Gain 7 [Credits]")
    (click-prompt state :corp "Corp Basic Action Card - [Click]: Gain 1 [Credits]")
    ))
