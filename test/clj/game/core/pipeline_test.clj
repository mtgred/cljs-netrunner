(ns game.core.pipeline-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [game.core.pipeline :as sut]
            [clojure.test :refer :all]))

(deftest continue-gp-test
  (testing "nothing stopping performing multiple steps in a row"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand []
                        :credits 10}})
      (is (= 0 (count (:hand (get-corp)))))
      (let [step (sut/make-step #(core/draw state :corp % 1 nil))]
        (sut/queue-step state step)
        (sut/queue-step state step))
      (sut/continue-gp state)
      (is (nil? (sut/get-current-step state)))
      (is (= 2 (count (:hand (get-corp)))))))
  (testing "a card ability pausing recurring"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Jinja City Grid"]
                        :credits 10}})
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (is (= 0 (count (:hand (get-corp)))))
      (let [step (sut/make-step #(core/draw state :corp % 1 nil))]
        (sut/queue-step state step)
        (sut/queue-step state step))
      (sut/continue-gp state)
      (is (sut/get-current-step state))
      (is (= 1 (count (:hand (get-corp)))))
      (click-prompt state :corp "None")
      (is (sut/get-current-step state))
      (click-prompt state :corp "None")
      (is (empty? (:prompt (get-corp))))
      (is (nil? (sut/get-current-step state)))
      )))
