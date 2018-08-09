(ns game-test.engine.card-definitions
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.cards :refer [all-cards card-definitions]]
            [tasks.utils :refer [type->dir]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(defn make-label
  [ab]
  (seq (utils/make-label ab)))

(deftest displayed-abilities-require-lables
  (doseq [[title card] (sort-by #(type->dir (val %)) @all-cards)
          :let [card (@card-definitions title)
                abilities (:abilities card)
                subroutines (:subroutines card)
                runner-abilities (:runner-abilities card)
                corp-abilities (:corp-abilities card)]]
    (when abilities
      (doseq [[idx ab] (map-indexed vector abilities)]
        (is (make-label ab) (str title " ability " idx " needs a label"))))
    (when subroutines
      (doseq [[idx sub] (map-indexed vector subroutines)]
        (is (make-label sub) (str title " subroutine " idx " needs a label"))))
    (when runner-abilities
      (doseq [[idx ab] (map-indexed vector runner-abilities)]
        (is (make-label ab) (str title " runner-ability " idx " needs a label"))))
    (when corp-abilities
      (doseq [[idx ab] (map-indexed vector corp-abilities)]
        (is (make-label ab) (str title " corp-ability " idx " needs a label"))))))