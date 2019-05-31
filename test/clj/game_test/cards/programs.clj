(ns game-test.cards.programs
  (:require [game.core :as core]
            [game.core.card-properties :refer :all]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adept
  ;; Adept - +1 str for each unused MU
  (do-game
    (new-game {:runner {:deck ["Adept" "Box-E"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Adept")
    (let [ad (get-program state 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 4 (:current-strength (refresh ad))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 6 (:current-strength (refresh ad))) "+4 strength for 4 unused MU"))))

(deftest algernon
  ;; Algernon - pay 2 credits to gain a click, trash if no successful run
  (testing "Use, successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (run-successful state)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed")))
  (testing "Use, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Algernon trashed")
      (is (empty? (get-program state)) "No programs installed")))
  (testing "Not used, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "No")
      (is (= 8 (:credit (get-runner))) "No credits spent")
      (is (= 4 (:click (get-runner))) "No clicks gained")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed"))))

(deftest amina
  ;; Amina ability
  (do-game
    (new-game {:runner {:deck ["Amina"]}
               :corp {:deck ["Enigma"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Amina")
    (let [amina (get-program state 0)
          enigma (get-ice state :hq 0)]
      (run-on state :hq)
      (core/rez state :corp (refresh enigma))
      (is (= 4 (:credit (get-corp))))
      (card-ability state :runner (refresh amina) 2)
      (is (= 3 (:credit (get-corp))) "Corp lost 1 credit")
      (card-ability state :runner (refresh amina) 2)
      (is (= 3 (:credit (get-corp))) "Can only use ability once per turn"))))

(deftest atman
  ;; Atman
  (testing "Installing with 0 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "0")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (zero? (get-counters atman :power)) "0 power counters")
        (is (zero? (:current-strength atman)) "0 current strength"))))
  (testing "Installing with 2 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "2")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (= 2 (get-counters atman :power)) "2 power counters")
        (is (= 2 (:current-strength atman)) "2 current strength")))))

(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game {:runner {:deck [(qty "Au Revoir" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :corp nil)
    (core/jack-out state :runner nil)
    (is (= 5 (:credit (get-runner))) "Gained 1 credit from jacking out")
    (play-from-hand state :runner "Au Revoir")
    (run-on state "Archives")
    (core/no-action state :corp nil)
    (core/jack-out state :runner nil)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit from each copy of Au Revoir")))

(deftest aumakua
  ;; Aumakua - Gain credit on no-trash
  (testing "Gain counter on no trash"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 3)]}
                 :runner {:deck ["Aumakua"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from no-trash")
      (core/gain state :runner :credit 5)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from trash")))
  (testing "Gain counters on empty archives"
    (do-game
      (new-game {:runner {:deck ["Aumakua"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Aumakua")
      (run-empty-server state :archives)
      (is (= 1 (get-counters (get-program state 0) :virus)) "Aumakua gains virus counter from accessing empty Archives")))
  (testing "Neutralize All Threats interaction"
    (do-game
      (new-game {:corp {:deck [(qty "PAD Campaign" 3)]}
                 :runner {:deck ["Aumakua" "Neutralize All Threats"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Neutralize All Threats")
      (core/gain state :runner :credit 5)
      (run-empty-server state "Server 1")
      (is (zero? (get-counters (get-program state 0) :virus)) "Aumakua does not gain virus counter from ABT-forced trash"))))

(deftest baba-yaga
  ;; Baba Yaga
  (do-game
    (new-game {:runner {:deck ["Baba Yaga" "Faerie" "Yog.0" "Sharpshooter"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Baba Yaga")
    (play-from-hand state :runner "Sharpshooter")
    (let [baba (get-program state 0)
          base-abicount (count (:abilities baba))]
      (card-ability state :runner baba 0)
      (click-card state :runner (find-card "Faerie" (:hand (get-runner))))
      (is (= (+ 2 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 2 subroutines from Faerie")
      (card-ability state :runner (refresh baba) 0)
      (click-card state :runner (find-card "Yog.0" (:hand (get-runner))))
      (is (= (+ 3 base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga gained 1 subroutine from Yog.0")
      (core/trash state :runner (first (:hosted (refresh baba))))
      (is (= (inc base-abicount) (count (:abilities (refresh baba)))) "Baba Yaga lost 2 subroutines from trashed Faerie")
      (card-ability state :runner baba 1)
      (click-card state :runner (find-card "Sharpshooter" (:program (:rig (get-runner)))))
      (is (= 2 (count (:hosted (refresh baba)))) "Faerie and Sharpshooter hosted on Baba Yaga")
      (is (= 1 (core/available-mu state)) "1 MU left with 2 breakers on Baba Yaga")
      (is (= 4 (:credit (get-runner))) "-5 from Baba, -1 from Sharpshooter played into Rig, -5 from Yog"))))

(deftest bankroll
  ;; Bankroll
  (do-game
    (new-game {:runner {:deck ["Bankroll"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bankroll")
    (is (= 3 (core/available-mu state)) "Bankroll uses up 1 MU")
    (is (= 4 (:credit (get-runner))) "Bankroll cost 1 to install")
    (let [bankroll (get-program state 0)
          hosted-credits #(get-counters (refresh bankroll) :credit)]
      (is (zero? (hosted-credits)) "No counters on Bankroll on install")
      (run-empty-server state "Archives")
      (is (= 1 (hosted-credits)) "One credit counter on Bankroll after one successful run")
      (run-empty-server state "R&D")
      (is (= 2 (hosted-credits)) "Two credit counter on Bankroll after two successful runs")
      (run-empty-server state "HQ")
      (is (= 3 (hosted-credits)) "Three credit counter on Bankroll after three successful runs")
      (card-ability state :runner bankroll 0)
      (is (= (+ 4 3) (:credit (get-runner))) "Gained 3 credits when trashing Bankroll")
      (is (= 1 (-> (get-runner) :discard count)) "Bankroll was trashed"))))

(deftest berserker
  ;; Berserker
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Hive" "Enigma"]
                      :credits 100}
               :runner {:hand ["Berserker"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Hive" "R&D")
    (play-from-hand state :corp "Enigma" "HQ")
    (core/rez state :corp (get-ice state :archives 0))
    (core/rez state :corp (get-ice state :rd 0))
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Berserker")
    (let [berserker (get-program state 0)]
      (is (= 2 (core/get-strength (refresh berserker))) "Berserker strength starts at 2")
      (run-on state :archives)
      (card-ability state :runner berserker 0)
      (is (= 3 (core/get-strength (refresh berserker))) "Berserker gains 1 strength from Ice Wall")
      (core/jack-out state :runner nil)
      (is (= 2 (core/get-strength (refresh berserker))) "Berserker strength resets at end of run")
      (run-on state :rd)
      (card-ability state :runner berserker 0)
      (is (= 7 (core/get-strength (refresh berserker))) "Berserker gains 5 strength from Hive")
      (core/jack-out state :runner nil)
      (is (= 2 (core/get-strength (refresh berserker))) "Berserker strength resets at end of run")
      (run-on state :hq)
      (card-ability state :runner berserker 0)
      (is (= 2 (core/get-strength (refresh berserker))) "Berserker gains 0 strength from Enigma (non-barrier)"))))

(deftest bukhgalter
  ;; Bukhgalter ability
  (do-game
    (new-game {:runner {:deck ["Bukhgalter"]}
               :corp {:deck ["Pup"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Bukhgalter")
    (let [bukhgalter (get-program state 0)
          pup (get-ice state :hq 0)]
      (run-on state :hq)
      (core/rez state :corp (refresh pup))
      (is (= 2 (:credit (get-runner))))
      (card-ability state :runner (refresh bukhgalter) 2)
      (is (= 4 (:credit (get-runner))) "Gained 2 credits")
      (card-ability state :runner (refresh bukhgalter) 2)
      (is (= 4 (:credit (get-runner))) "Can only use ability once per turn"))))

(deftest cerberus-rex-h2
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
    (new-game {:runner {:deck ["Cerberus \"Rex\" H2"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cerberus \"Rex\" H2")
    (is (= 2 (:credit (get-runner))) "2 credits left after install")
    (let [rex (get-program state 0)]
      (is (= 4 (get-counters rex :power)) "Start with 4 counters")
      ;; boost strength
      (card-ability state :runner rex 1)
      (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
      (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
      ;; break
      (card-ability state :runner rex 0)
      (is (= 1 (:credit (get-runner))) "No credits spent to break")
      (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))

(deftest chameleon
  ;; Chameleon - Install on corp turn, only returns to hand at end of runner's turn
  (testing "with Clone Chip"
    (do-game
      (new-game {:runner {:deck ["Chameleon" "Clone Chip"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (core/move state :runner (find-card "Chameleon" (:hand (get-runner))) :discard)
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))))
      ;; Install Chameleon on corp turn
      (take-credits state :corp 1)
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Chameleon" (:discard (get-runner))))
        (click-prompt state :runner "Sentry"))
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Chameleon not returned to hand at end of corp turn")
      (take-credits state :runner)
      (is (= 1 (count (:hand (get-runner)))) "Chameleon returned to hand at end of runner's turn")))
  (testing "Returns to hand after hosting. #977"
    (do-game
      (new-game {:runner {:deck [(qty "Chameleon" 2) "Scheherazade"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Chameleon")
      (click-prompt state :runner "Barrier")
      (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon")
      ;; Host the Chameleon on Scheherazade that was just played (as in Personal Workshop/Hayley ability scenarios)
      (play-from-hand state :runner "Scheherazade")
      (let [scheherazade (get-program state 1)]
        (card-ability state :runner scheherazade 1) ; Host an installed program
        (click-card state :runner (find-card "Chameleon" (:program (:rig (get-runner)))))
        (is (= 4 (:credit (get-runner))) "+1 from hosting onto Scheherazade")
        ;; Install another Chameleon directly onto Scheherazade
        (card-ability state :runner scheherazade 0) ; Install and host a program from Grip
        (click-card state :runner (find-card "Chameleon" (:hand (get-runner))))
        (click-prompt state :runner "Code Gate")
        (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
        (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
      (is (zero? (count (:hand (get-runner)))) "Both Chameleons in play - hand size 0")
      (take-credits state :runner)
      (is (= 2 (count (:hand (get-runner)))) "Both Chameleons returned to hand - hand size 2"))))

(deftest chisel
  ;; Chisel
  (testing "Basic test"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Chisel"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Chisel")
      (click-card state :runner "Ice Wall")
      (let [iw (get-ice state :hq 0)
            chisel (first (:hosted (refresh iw)))]
        (run-on state "HQ")
        (core/rez state :corp iw)
        (is (zero? (get-counters (refresh chisel) :virus)))
        (card-ability state :runner chisel 0)
        (is (= 1 (get-counters (refresh chisel) :virus)))
        (is (refresh iw) "Ice Wall still around, just at 0 strength")
        (core/jack-out state :runner nil)
        (run-on state "HQ")
        (card-ability state :runner chisel 0)
        (is (nil? (refresh iw)) "Ice Wall should be trashed")
        (is (nil? (refresh chisel)) "Chisel should likewise be trashed"))))
  (testing "Doesn't work if the ice is unrezzed"
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Chisel"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Chisel")
      (click-card state :runner "Ice Wall")
      (let [iw (get-ice state :hq 0)
            chisel (first (:hosted (refresh iw)))]
        (run-on state "HQ")
        (is (zero? (get-counters (refresh chisel) :virus)))
        (card-ability state :runner chisel 0)
        (is (zero? (get-counters (refresh chisel) :virus)) "Chisel gains no counters as Ice Wall is unrezzed")
        (is (refresh iw) "Ice Wall still around, still at 1 strength")
        (core/jack-out state :runner nil)
        (run-on state "HQ")
        (core/rez state :corp iw)
        (card-ability state :runner chisel 0)
        (is (= 1 (get-counters (refresh chisel) :virus)) "Chisel now has 1 counter")
        (core/jack-out state :runner nil)
        (run-on state "HQ")
        (core/derez state :corp iw)
        (card-ability state :runner chisel 0)
        (is (refresh iw) "Ice Wall should still be around as it's unrezzed")))))

(deftest consume
  ;; Consume - gain virus counter for trashing corp card. click to get 2c per counter.
  (testing "Trash and cash out"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (let [c (get-program state 0)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters"))))
  (testing "Hivemind interaction"
    (do-game
      (new-game {:corp {:deck ["Adonis Campaign"]}
                 :runner {:deck ["Consume" "Hivemind"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 3)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:discard (get-corp)))) "Adonis Campaign trashed")
        (is (= 1 (get-counters (refresh c) :virus)) "Consume gains a counter")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind retains counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 4 (:credit (get-runner))) "Runner gains 4 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters"))))
  (testing "Hivemind counters only"
    (do-game
      (new-game {:runner {:deck ["Consume" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Consume")
      (play-from-hand state :runner "Hivemind")
      (let [c (get-program state 0)
            h (get-program state 1)]
        (is (zero? (get-counters (refresh c) :virus)) "Consume starts with no counters")
        (is (= 1 (get-counters (refresh h) :virus)) "Hivemind starts with a counter")
        (is (zero? (:credit (get-runner))) "Runner starts with no credits")
        (card-ability state :runner c 0)
        (is (= 2 (:credit (get-runner))) "Runner gains 2 credits")
        (is (zero? (get-counters (refresh c) :virus)) "Consume loses counters")
        (is (zero? (get-counters (refresh h) :virus)) "Hivemind loses counters")))))

(deftest cradle
  ;; Cradle
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Cradle" (qty "Cache" 100)]}})
    (starting-hand state :runner ["Cradle"])
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 100 :click 100)
    (play-from-hand state :runner "Cradle")
    (run-on state "HQ")
    (let [cradle (get-program state 0)
          strength (:strength (refresh cradle))]
      (dotimes [n 5]
        (when (pos? n)
          (core/draw state :runner n))
        (is (= (- strength n) (:current-strength (refresh cradle))) (str "Cradle should lose " n " strength"))
        (starting-hand state :runner [])
        (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))
      (core/draw state :runner 1)
      (is (= (dec strength) (:current-strength (refresh cradle))) "Cradle should lose 1 strength")
      (play-from-hand state :runner "Cache")
      (is (= strength (:current-strength (refresh cradle))) (str "Cradle should be back to original strength")))))

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game {:corp {:deck ["Quandary"]}
               :runner {:deck ["Crescentus"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Crescentus")
    (run-on state "HQ")
    (let [cres (get-program state 0)
          q (get-ice state :hq 0)]
      (card-ability state :runner cres 0)
      (is (not (nil? (get-program state 0))) "Crescentus could not be used because the ICE is not rezzed")
      (core/rez state :corp q)
      (is (:rezzed (refresh q)) "Quandary is now rezzed")
      (card-ability state :runner cres 0)
      (is (nil? (get-program state 0)) "Crescentus could be used because the ICE is rezzed")
      (is (not (:rezzed (refresh q))) "Quandary is no longer rezzed"))))

(deftest crypsis
  ;; Crypsis - Loses a virus counter after encountering ice it broke
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Crypsis" 2)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (card-ability state :runner crypsis 2)
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (= 1 (get-counters (refresh crypsis) :virus))
          "Crypsis has 1 virus counter")
      (run-continue state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has 0 virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-program state 0)]
      (run-on state "Archives")
      (card-ability state :runner (refresh crypsis) 0) ; Match strength
      (card-ability state :runner (refresh crypsis) 1) ; Break
      (is (zero? (get-counters (refresh crypsis) :virus))
          "Crypsis has nil virus counters")
      (run-jack-out state)
      (is (= "Crypsis" (:title (first (:discard (get-runner)))))
          "Crypsis was trashed"))))

(deftest darwin
  ;; Darwin - starts at 0 strength
  (do-game
    (new-game {:runner {:deck ["Darwin"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Darwin")
    (let [darwin (get-program state 0)]
      (is (zero? (get-counters (refresh darwin) :virus)) "Darwin starts with 0 virus counters")
      (is (zero? (:current-strength (refresh darwin))) "Darwin starts at 0 strength")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (refresh darwin) 1) ; add counter
      (is (= 1 (get-counters (refresh darwin) :virus)) "Darwin gains 1 virus counter")
      (is (= 1 (:current-strength (refresh darwin))) "Darwin is at 1 strength"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered ICE
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Datasucker"]}})
      (play-from-hand state :corp "Fire Wall" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (play-from-hand state :runner "Datasucker")
      (let [ds (get-program state 0)
            fw (get-ice state :remote1 0)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh ds) :virus)))
        (run-empty-server state "Archives")
        (is (= 2 (get-counters (refresh ds) :virus)))
        (run-on state "Server 1")
        (run-continue state)
        (run-successful state)
        (is (= 2 (get-counters (refresh ds) :virus)) "No counter gained, not a central server")
        (run-on state "Server 1")
        (core/rez state :corp fw)
        (is (= 5 (:current-strength (refresh fw))))
        (card-ability state :runner ds 0)
        (is (= 1 (get-counters (refresh ds) :virus)) "1 counter spent from Datasucker")
        (is (= 4 (:current-strength (refresh fw))) "Fire Wall strength lowered by 1"))))
  (testing "does not affect next ice when current is trashed. Issue #1788"
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:deck ["Datasucker" "Parasite"]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (play-from-hand state :runner "Datasucker")
      (let [sucker (get-program state 0)
            spider (get-ice state :hq 0)
            wrap (get-ice state :hq 1)]
        (core/add-counter state :runner sucker :virus 2)
        (core/rez state :corp spider)
        (core/rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner "Spiderweb")
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner (refresh sucker) 0)
        (card-ability state :runner (refresh sucker) 0)
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Datasucker")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker")))))

(deftest deus-x
  (testing "vs Multiple Hostile Infrastructure"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3)]}
                 :runner {:deck [(qty "Deus X" 3) (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/gain state :corp :credit 10)
      (core/rez state :corp (get-content state :remote1 0))
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Deus X")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (let [dx (get-program state 0)]
        (card-ability state :runner dx 1)
        (click-prompt state :runner "Done")
        (is (= 2 (count (:hand (get-runner)))) "Deus X prevented one Hostile net damage"))))
  (testing "vs Multiple sources of net damage"
    (do-game
      (new-game {:corp {:id "Jinteki: Personal Evolution"
                        :deck [(qty "Fetal AI" 6)]}
                 :runner {:deck [(qty "Deus X" 3) (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Fetal AI" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Deus X")
      (run-empty-server state "Server 1")
      (let [dx (get-program state 0)]
        (card-ability state :runner dx 1)
        (click-prompt state :runner "Done")
        (click-prompt state :runner "Pay to steal")
        (is (= 3 (count (:hand (get-runner)))) "Deus X prevented net damage from accessing Fetal AI, but not from Personal Evolution")
        (is (= 1 (count (:scored (get-runner)))) "Fetal AI stolen")))))

(deftest dhegdheer
  ;; Dheghdheer - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game {:runner {:deck ["Adept" "Dhegdheer"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dhegdheer")
    (play-from-hand state :runner "Adept")
    (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
    (is (= 2 (core/available-mu state)) "2 MU used")
    (let [dheg (get-program state 0)
          adpt (get-program state 1)]
      (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
      (card-ability state :runner dheg 1)
      (click-card state :runner (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh dheg)))]
        (is (= 4 (:credit (get-runner))) "4 credits left after hosting")
        (is (= 4 (core/available-mu state)) "0 MU used")
        (is (= 6 (:current-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest disrupter
  ;; Disrupter
  (do-game
    (new-game {:corp {:deck [(qty "SEA Source" 2)]}
               :runner {:deck ["Disrupter"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Disrupter")
    (take-credits state :runner)
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :runner "Yes")
    (is (zero? (-> (get-corp) :prompt first :base)) "Base trace should now be 0")
    (is (= 1 (-> (get-runner) :discard count)) "Disrupter should be in Heap")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (zero? (count-tags state)) "Runner should gain no tag from beating trace")
    (play-from-hand state :corp "SEA Source")
    (is (= 3 (-> (get-corp) :prompt first :base)) "Base trace should be reset to 3")))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)]}
               :runner {:deck ["Diwan"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Diwan")
    (click-prompt state :runner "HQ")
    (take-credits state :runner)
    (is (= 8 (:credit (get-corp))) "8 credits for corp at start of second turn")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (is (= 8 (:credit (get-corp))) "Diwan did not charge extra for install on another server")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 7 (:credit (get-corp))) "Diwan charged 1cr to install ice protecting the named server")
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (is (= 7 (:credit (get-corp))) "Diwan didn't charge to install another upgrade in root of HQ")
    (take-credits state :corp)
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (is (= 5 (:credit (get-corp))) "Diwan charged 1cr + 1cr to install a second ice protecting the named server")
    (core/gain state :corp :click 1)
    (core/purge state :corp)
    (play-from-hand state :corp "Fire Wall" "HQ") ; 2cr cost from normal install cost
    (is (= "Diwan" (-> (get-runner) :discard first :title)) "Diwan was trashed from purge")
    (is (= 3 (:credit (get-corp))) "No charge for installs after Diwan purged")))

(deftest djinn
  ;; Djinn
  (testing "Hosted Chakana does not disable advancing agendas. Issue #750"
    (do-game
      (new-game {:corp {:deck ["Priority Requisition"]}
                 :runner {:deck ["Djinn" "Chakana"]}})
      (play-from-hand state :corp "Priority Requisition" "New remote")
      (take-credits state :corp 2)
      (play-from-hand state :runner "Djinn")
      (let [djinn (get-program state 0)
            agenda (get-content state :remote1 0)]
        (is agenda "Agenda was installed")
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (let [chak (first (:hosted (refresh djinn)))]
          (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
          ;; manually add 3 counters
          (core/add-counter state :runner (first (:hosted (refresh djinn))) :virus 3)
          (take-credits state :runner 2)
          (core/advance state :corp {:card agenda})
          (is (= 1 (get-counters (refresh agenda) :advancement)) "Agenda was advanced")))))
  (testing "Host a non-icebreaker program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Chakana"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (is (= 3 (core/available-mu state)))
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 1)
        (click-card state :runner (find-card "Chakana" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "No memory used to host on Djinn")
        (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
        (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))
  (testing "Tutor a virus program"
    (do-game
      (new-game {:runner {:deck ["Djinn" "Parasite"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Djinn")
      (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
      (let [djinn (get-program state 0)]
        (card-ability state :runner djinn 0)
        (click-prompt state :runner (find-card "Parasite" (:deck (get-runner))))
        (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
        (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
        (is (= 2 (:click (get-runner))) "1click to use Djinn ability")))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game {:corp {:deck [(qty "Restructure" 3) (qty "Hedge Fund" 3)]}
               :runner {:id "Laramy Fisk: Savvy Investor"
                        :deck ["Equivocation" "Desperado"]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Equivocation")
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :rd)
    (click-prompt state :runner "Laramy Fisk: Savvy Investor")
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Corp forced to draw by Fisk")
    (click-prompt state :runner "Yes") ; Equivocation prompt
    (click-prompt state :runner "Yes") ; force the draw
    (is (= 1 (:credit (get-runner))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-corp)))) "Corp forced to draw by Equivocation")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest faerie
  ;; Faerie - trash after encounter is over, not before.
  (do-game
    (new-game {:corp {:deck ["Caduceus"]}
               :runner {:deck ["Faerie"]}})
    (play-from-hand state :corp "Caduceus" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Faerie")
    (let [fae (get-program state 0)]
      (run-on state :archives)
      (core/rez state :corp (get-ice state :archives 0))
      (card-ability state :runner fae 0)
      (is (refresh fae) "Faerie not trashed until encounter over")
      (run-continue state)
      (is (find-card "Faerie" (:discard (get-runner))) "Faerie trashed"))))

(deftest false-echo
  ;; False Echo - choice for Corp
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:deck [(qty "False Echo" 3)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "False Echo")
    (play-from-hand state :runner "False Echo")
    (run-on state "Archives")
    (run-continue state)
    (let [echo1 (get-program state 0)
          echo2 (get-program state 1)]
      (card-ability state :runner echo1 0)
      (click-prompt state :corp "Add to HQ")
      (is (= 2 (count (:hand (get-corp)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-runner)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :runner echo2 0)
      (click-prompt state :corp "Rez")
      (is (:rezzed (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-runner)))) "False Echo trashed"))))

(deftest faust
  (testing "Basic test: Pump by discarding"
    (do-game
      (new-game {:runner {:deck ["Faust" (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Faust")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 4 (:current-strength (refresh faust))) "4 current strength")
        (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))
  (testing "Pump does not trigger trash prevention. #760"
    (do-game
      (new-game {:runner {:deck ["Faust"
                                 "Sacrificial Construct"
                                 "Fall Guy"
                                 "Astrolabe"
                                 "Gordian Blade"
                                 "Armitage Codebusting"]}})
      (take-credits state :corp)
      (core/draw state :runner 1)
      (play-from-hand state :runner "Faust")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Sacrificial Construct")
      (is (= 2 (count (get-resource state))) "Resources installed")
      (let [faust (get-program state 0)]
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Astrolabe" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for hardware")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Gordian Blade" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for program")
        (card-ability state :runner faust 1)
        (click-card state :runner (find-card "Armitage Codebusting" (:hand (get-runner))))
        (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for resource")))))

(deftest femme-fatale
  ;; Femme Fatale counter test
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Femme Fatale" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 18)
    (let [iw (get-ice state :hq 0)]
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :runner (get-program state 0))
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
      (play-from-hand state :runner "Femme Fatale")
      (click-card state :runner iw)
      (is (:icon (refresh iw)) "Ice Wall has an icon")
      (core/trash state :corp iw)
      (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

(deftest gauss
  ;; Gauss
  (testing "Loses strength at end of Runner's turn"
    (do-game
      (new-game {:runner {:deck ["Gauss"]}
                 :options {:start-as :runner}})
      (play-from-hand state :runner "Gauss")
      (let [gauss (get-program state 0)]
        (is (= 4 (:current-strength (refresh gauss))) "+3 base strength")
        (run-on state :hq)
        (card-ability state :runner (refresh gauss) 1) ;; boost
        (is (= 6 (:current-strength (refresh gauss))) "+3 base and boosted strength")
        (run-jack-out state)
        (is (= 4 (:current-strength (refresh gauss))) "Boost lost after run")
        (take-credits state :runner)
        (is (= 1 (:current-strength (refresh gauss))) "Back to normal strength"))))
  (testing "Loses strength at end of Corp's turn"
    (do-game
      (new-game {:runner {:deck ["Gauss"]}})
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Gauss")
      (let [gauss (get-program state 0)]
        (is (= 4 (:current-strength (refresh gauss))) "+3 base strength")
        (take-credits state :corp)
        (is (= 1 (:current-strength (refresh gauss))) "Back to normal strength")))))

(deftest god-of-war
  ;; God of War - Take 1 tag to place 2 virus counters
  (do-game
    (new-game {:runner {:deck ["God of War"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "God of War")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [gow (get-program state 0)]
      (card-ability state :runner gow 2)
      (is (= 1 (count-tags state)))
      (is (= 2 (get-counters (refresh gow) :virus)) "God of War has 2 virus counters"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Corp cards are trashed, spend click-counter to mill Corp
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 2) (qty "Enigma" 2)]}
               :runner {:deck ["Gravedigger"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gravedigger")
    (let [gd (get-program state 0)]
      (core/trash state :corp (get-content state :remote1 0))
      (is (= 1 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/trash state :corp (get-content state :remote2 0))
      (is (= 2 (get-counters (refresh gd) :virus)) "Gravedigger gained 1 counter")
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Enigma" (:hand (get-corp))) :deck)
      (is (= 2 (count (:deck (get-corp)))))
      (card-ability state :runner gd 0)
      (is (= 1 (get-counters (refresh gd) :virus)) "Spent 1 counter from Gravedigger")
      (is (= 2 (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:deck (get-corp)))))
      (is (= 3 (count (:discard (get-corp)))) "Milled 1 card from R&D"))))

(deftest harbinger
  ;; Harbinger
  (testing "install facedown when Blacklist installed"
    (do-game
      (new-game {:corp {:deck ["Blacklist"]}
                 :runner {:deck ["Harbinger"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Harbinger")
      (core/trash state :runner (-> (get-runner) :rig :program first))
      (is (zero? (count (:discard (get-runner)))) "Harbinger not in heap")
      (is (-> (get-runner) :rig :facedown first :facedown) "Harbinger installed facedown"))))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hyperdriver")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (get-program state 0)]
        (card-ability state :runner hyp 0)
        (core/end-phase-12 state :runner nil)
        (is (= 7 (:click (get-runner))) "Gained 3 clicks")
        (is (= 1 (count (:rfg (get-runner)))) "Hyperdriver removed from game"))))
  (testing "triggering a Dhegdeered Hyperdriver should not grant +3 MU"
    (do-game
      (new-game {:runner {:deck ["Hyperdriver" "Dhegdheer"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Dhegdheer")
      (let [dheg (get-program state 0)]
        (card-ability state :runner dheg 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "0 MU used by Hyperdriver hosted on Dhegdheer")
        (is (= 2 (:click (get-runner))) "2 clicks used")
        (is (= 3 (:credit (get-runner))) "2 credits used")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (let [hyp (first (:hosted (refresh dheg)))]
          (card-ability state :runner hyp 0)
          (core/end-phase-12 state :runner nil)
          (is (= 7 (:click (get-runner))) "Used Hyperdriver")
          (is (= 4 (core/available-mu state)) "Still 0 MU used after Hyperdriver removed from game"))))))

(deftest ika
  ;; Ika
  (testing "Can be hosted on both rezzed/unrezzed ice, respects no-host, is blanked by Magnet"
    (do-game
      (new-game {:corp {:deck ["Tithonium" "Enigma" "Magnet"]}
                 :runner {:deck ["Ika"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Tithonium" "Archives")
      (play-from-hand state :corp "Magnet" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Ika")
      (core/gain state :runner :credit 100)
      (core/gain state :corp :credit 100)
      (let [ika (get-program state 0)
            enigma (get-ice state :hq 0)
            tithonium (get-ice state :archives 0)
            magnet (get-ice state :rd 0)]
        (let [creds (:credit (get-runner))]
          (card-ability state :runner ika 2) ; host on a piece of ice
          (click-card state :runner tithonium)
          (is (utils/same-card? ika (first (:hosted (refresh tithonium)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from rig cost 2 creds"))
        (run-on state :archives)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh tithonium)))]
          (card-ability state :runner ika 2)
          (click-card state :runner enigma)
          (is (utils/same-card? ika (first (:hosted (refresh enigma)))) "Ika was rehosted")
          (is (= (- creds 2) (:credit (get-runner))) "Rehosting from ice during run cost 2 creds"))
        (core/rez state :corp tithonium)
        (let [creds (:credit (get-runner))
              ika (first (:hosted (refresh enigma)))]
          (card-ability state :runner ika 2)
          (click-card state :runner tithonium)
          (is (zero?(count (:hosted (refresh tithonium)))) "Ika was not hosted on Tithonium")
          (is (= creds (:credit (get-runner))) "Clicking invalid targets is free")
          (click-prompt state :runner "Done")
          (core/rez state :corp magnet)
          (click-card state :corp ika)
          (is (zero?(count (:hosted (refresh enigma)))) "Ika was removed from Enigma")
          (is (= 1 (count (:hosted (refresh magnet)))) "Ika was hosted onto Magnet")
          (let [ika (first (:hosted (refresh magnet)))]
            (is (zero?(count (:abilities ika))) "Ika was blanked")))))))

(deftest imp
  ;; Imp
  (testing "Full test"
    (letfn [(imp-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:deck ["Imp"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Imp")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Imp]: Trash card")
                (is (= 1 (count (:discard (get-corp)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Ice Wall"
                   "Oberth Protocol"]))))
  (testing "vs an ambush"
    (do-game
      (new-game {:corp {:deck ["Prisec"]}
                 :runner {:deck ["Imp" (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            tags (count-tags state)
            grip (count (:hand (get-runner)))
            archives (count (:discard (get-corp)))]
        (play-from-hand state :runner "Imp")
        (run-empty-server state :remote1)
        (click-prompt state :corp "Yes")
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= 2 (- credits (:credit (get-corp)))) "Corp paid 2 for Prisec")
        (is (= 1 (- (count-tags state) tags)) "Runner has 1 tag")
        (is (= 2 (- grip (count (:hand (get-runner))))) "Runner took 1 meat damage")
        (is (= 1 (- (count (:discard (get-corp))) archives)) "Used Imp to trash Prisec"))))
  (testing "vs The Future Perfect"
    ;; Psi-game happens on access [5.5.1], Imp is a trash ability [5.5.2]
    (do-game
      (new-game {:corp {:deck ["The Future Perfect"]}
                 :runner {:deck ["Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (testing "Access, corp wins psi-game"
        (run-empty-server state "HQ")
        ;; Should access TFP at this point
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (click-prompt state :runner "[Imp]: Trash card")
        (take-credits state :runner)
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP")
        (core/move state :corp (find-card "The Future Perfect" (:discard (get-corp))) :hand))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Access, runner wins psi-game"
        (run-empty-server state "HQ")
        ;; Access prompt for TFP
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        ;; Fail psi game
        (click-prompt state :runner "[Imp]: Trash card")
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (zero? (:agenda-point (get-runner))) "Runner did not steal TFP"))))
  (testing "vs cards in Archives"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:deck ["Imp"]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :discard)
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (run-empty-server state "Archives")
      (is (= ["Steal"] (->> (get-runner) :prompt first :choices)) "Should only get the option to steal Hostile on access in Archives"))))

(deftest incubator
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus program
  (do-game
    (new-game {:runner {:deck ["Incubator" "Datasucker"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (play-from-hand state :runner "Incubator")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [ds (get-program state 0)
          incub (get-program state 1)]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :runner incub 0)
      (click-card state :runner ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-program state))))
      (is (= 1 (count (:discard (get-runner)))) "Incubator trashed")
      (is (= 3 (:click (get-runner)))))))

(deftest inversificator
  ;; Inversificator shouldn't hook up events for unrezzed ice
  (do-game
    (new-game {:corp {:deck ["Turing" "Kakugo"]}
               :runner {:deck ["Inversificator" "Sure Gamble"]}})
    (play-from-hand state :corp "Kakugo" "HQ")
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Inversificator")
    (let [inv (get-program state 0)
          tur (get-ice state :hq 1)]
      (is (= 1 (count (:hand (get-runner)))) "Runner starts with 1 card in hand")
      (run-on state :hq)
      (core/rez state :corp (refresh tur))
      (run-continue state)
      (card-ability state :runner (refresh inv) 0)
      (click-card state :runner (get-ice state :hq 1))
      (click-card state :runner (get-ice state :hq 0))
      (run-jack-out state)
      (is (= 1 (count (:hand (get-runner)))) "Runner still has 1 card in hand")
      (run-on state :hq)
      (run-continue state)
      (is (= 1 (count (:hand (get-runner)))) "Kakugo doesn't fire when unrezzed"))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game {:corp {:deck ["Snowflake"]}
               :runner {:deck ["Ixodidae" "Lamprey"]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp at 7 credits")
    (play-from-hand state :runner "Ixodidae")
    (play-from-hand state :runner "Lamprey")
    (is (= 3 (:credit (get-runner))) "Runner paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-ice state :hq 0)]
      (core/rez state :corp s)
      (card-subroutine state :corp s 0)
      (is (prompt-is-card? state :corp s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? state :runner s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-corp))) "Corp paid 1 credit to rezz Snowflake")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-runner))) "Runner did not gain 1 credit from Ixodidae when corp spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-corp))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-runner))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))

(deftest kyuban
  ;; Kyuban
  (testing "Gain creds when passing a piece of ice, both when rezzed and when unrezzed."
    (do-game
      (new-game {:corp {:deck [(qty "Lockdown" 3)]}
                 :runner {:deck [(qty "Kyuban" 1)]}})
      (play-from-hand state :corp "Lockdown" "HQ")
      (play-from-hand state :corp "Lockdown" "Archives")
      (let [ld1 (get-ice state :archives 0)
            ld2 (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner ld1)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= starting-creds (:credit (get-runner))) "Gained no money for passing other ice")
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Gained 2 creds for passing unrezzed host ice"))
        (let [starting-creds-2 (:credit (get-runner))]
          (core/jack-out state :runner nil)
          (run-on state "Archives")
          (core/rez state :corp ld1)
          (core/no-action state :corp nil)
          (run-continue state)
          (is (= (+ starting-creds-2 2) (:credit (get-runner)))
              "Gained 2 creds for passing rezzed host ice")))))
  (testing "HB: Architects of Tomorrow interaction"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                        :deck ["Eli 1.0"]}
                 :runner {:deck ["Kyuban"]}})
      (play-from-hand state :corp "Eli 1.0" "HQ")
      (let [eli (get-ice state :hq 0)]
        (core/rez state :corp eli)
        (take-credits state :corp)
        (play-from-hand state :runner "Kyuban")
        (click-card state :runner eli)
        (let [starting-creds (:credit (get-runner))]
          (run-on state "HQ")
          (core/no-action state :corp nil)
          (core/continue state :runner nil)
          (click-prompt state :corp "Done")
          (core/continue state :runner nil)
          (is (= (+ starting-creds 2) (:credit (get-runner)))
              "Only gained 2 credits for passing Eli"))))))

(deftest lamprey
  ;; Lamprey - Corp loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game {:runner {:deck ["Lamprey"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lamprey")
    (let [lamp (get-program state 0)]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 6 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 5 (:credit (get-corp))) "Corp lost 1 credit")
      (take-credits state :runner)
      (core/purge state :corp)
      (is (empty? (get-program state)) "Lamprey trashed by purge"))))

(deftest leprechaun
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Adept" "Leprechaun"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Leprechaun")
      (play-from-hand state :runner "Adept")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (let [lep (get-program state 0)
            adpt (get-program state 1)]
        (is (= 3 (:current-strength (refresh adpt))) "Adept at 3 strength individually")
        (card-ability state :runner lep 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh lep)))]
          (is (= 3 (core/available-mu state)) "1 MU used")
          (is (= 5 (:current-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Leprechaun" "Hyperdriver" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leprechaun")
      (let [lep (get-program state 0)]
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not deducted from available MU")
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Imp" (:hand (get-runner))))
        (is (= 1 (:click (get-runner))))
        (is (zero? (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Imp 1 MU not deducted from available MU")
        ;; Trash Hyperdriver
        (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not added to available MU")
        (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
        (is (= 3 (core/available-mu state)) "Imp 1 MU not added to available MU")))))

(deftest magnum-opus
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game {:runner {:deck ["Magnum Opus"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (core/available-mu state)))
    (is (zero? (:credit (get-runner))))
    (let [mopus (get-program state 0)]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest mammon
  ;; Mammon - Pay to add X power counters at start of turn, all removed at end of turn
  (do-game
    (new-game {:runner {:deck ["Mammon"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mammon")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [mam (get-program state 0)]
      (card-ability state :runner mam 0)
      (click-prompt state :runner "3")
      (is (= 2 (:credit (get-runner))) "Spent 3 credits")
      (is (= 3 (get-counters (refresh mam) :power)) "Mammon has 3 power counters")
      (take-credits state :runner)
      (is (zero? (get-counters (refresh mam) :power)) "All power counters removed"))))

(deftest musaazi
  ;; Musaazi gains virus counters on successful runs and can spend virus counters from any installed card
  (do-game
    (new-game {:corp {:deck ["Lancelot"]}
               :runner {:deck ["Musaazi" "Imp"]}})
    (play-from-hand state :corp "Lancelot" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Musaazi")
    (play-from-hand state :runner "Imp")
    (let [lancelot (get-ice state :hq 0)
          musaazi (get-program state 0)
          imp (get-program state 1)]
      (run-empty-server state "Archives")
      (is (= 1 (get-counters (refresh musaazi) :virus)) "Musaazi has 1 virus counter")
      (is (= 1 (:current-strength (refresh musaazi))) "Initial Musaazi strength")
      (is (= 2 (get-counters (refresh imp) :virus)) "Initial Imp virus counters")
      (run-on state "HQ")
      (core/rez state :corp lancelot)
      (card-ability state :runner musaazi 1) ; match strength
      (click-card state :runner imp)
      (is (= 1 (get-counters (refresh imp) :virus)) "Imp lost 1 virus counter to pump")
      (is (= 2 (:current-strength (refresh musaazi))) "Musaazi strength 2")
      (is (empty? (:prompt (get-runner))) "No prompt open")
      (card-ability state :runner musaazi 0)
      (click-card state :runner musaazi)
      (click-card state :runner imp)
      (click-prompt state :runner "Done")
      (is (zero?(get-counters (refresh imp) :virus)) "Imp lost its final virus counter")
      (is (zero?(get-counters (refresh imp) :virus)) "Musaazi lost its virus counter"))))

(deftest na-not-k
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Architect" "Eli 1.0"]}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            architect (get-ice state :hq 0)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/rez state :corp architect)
        (is (= 2 (:current-strength (refresh nanotk))) "1 ice on HQ")
        (card-subroutine state :corp (refresh architect) 1)
        (click-card state :corp (find-card "Eli 1.0" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))
  (testing "Strength adjusts accordingly when run redirected to another server"
    (do-game
      (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Crick" "Cortex Lock"]}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Cortex Lock" "HQ")
      (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
      (play-from-hand state :corp "Crick" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            susanoo (get-ice state :hq 1)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/rez state :corp susanoo)
        (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
        (card-subroutine state :corp (refresh susanoo) 0)
        (is (= 2 (:current-strength (refresh nanotk))) "1 ice on Archives")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength")))))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
               :runner {:deck ["Nyashia"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Nyashia")
    (run-on state "R&D")
    (run-successful state)
    (click-prompt state :runner "Yes")
    (is (= 2 (+ (get-in @state [:runner :rd-access])
                (core/access-bonus-count (:run @state) :rd))))))

(deftest origami
  ;; Origami - Increases Runner max hand size
  (do-game
    (new-game {:runner {:deck [(qty "Origami" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Origami")
    (is (= 6 (hand-size :runner)))
    (play-from-hand state :runner "Origami")
    (is (= 9 (hand-size :runner)) "Max hand size increased by 2 for each copy installed")))

(deftest overmind
  ;; Overmind - Start with counters equal to unused MU
  (do-game
    (new-game {:runner {:deck ["Overmind" (qty "Akamatsu Mem Chip" 2)]}})
    (take-credits state :corp)
    (take-credits state :runner 1)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 6 (core/available-mu state)))
    (play-from-hand state :runner "Overmind")
    (is (= 5 (core/available-mu state)))
    (let [ov (get-program state 0)]
      (is (= 5 (get-counters (refresh ov) :power)) "Overmind has 5 counters"))))

(deftest paintbrush
  ;; Paintbrush - Give rezzed ICE a chosen subtype until the end of the next run
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Paintbrush"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paintbrush")
    (is (= 2 (core/available-mu state)))
    (let [iwall (get-ice state :hq 0)
          pb (get-program state 0)]
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (is (= 3 (:click (get-runner))) "Ice Wall not rezzed, so no click charged")
      (click-prompt state :runner "Done") ; cancel out
      (core/rez state :corp iwall)
      (card-ability state :runner pb 0)
      (click-card state :runner iwall)
      (click-prompt state :runner "Code Gate")
      (is (= 2 (:click (get-runner))) "Click charged")
      (is (true? (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (false? (utils/has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest paperclip
  ;; Paperclip - prompt to install on encounter, but not if another is installed
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck [(qty "Paperclip" 2)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "Yes") ; install paperclip
      (run-continue state)
      (run-successful state)
      (is (not (:run @state)) "Run ended")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (is (empty? (:prompt (get-runner))) "No prompt to install second Paperclip")))
  (testing "firing on facedown ice shouldn't crash"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["Paperclip"]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "0")))
  (testing "do not show a second install prompt if user said No to first, when multiple are in heap"
    (do-game
      (new-game {:corp {:deck [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Paperclip" 3)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (trash-from-hand state :runner "Paperclip")
      (run-on state "Archives")
      (core/rez state :corp (get-ice state :archives 1))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (run-continue state)
      ;; we should get the prompt on a second ice even after denying the first.
      (core/rez state :corp (get-ice state :archives 0))
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip")
      (core/jack-out state :runner)
      ;; Run again, make sure we get the prompt to install again.
      (run-on state "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-runner))) "No additional prompts to rez other copies of Paperclip"))))

(deftest parasite
  (testing "Basic functionality: Gain 1 counter every Runner turn"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) (qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner wrap)
        (is (= 3 (core/available-mu state)) "Parasite consumes 1 MU")
        (let [psite (first (:hosted (refresh wrap)))]
          (is (zero? (get-counters psite :virus)) "Parasite has no counters yet")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (get-counters (refresh psite) :virus))
              "Parasite gained 1 virus counter at start of Runner turn")
          (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))
  (testing "Installed facedown w/ Apex"
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Parasite"]}})
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (click-card state :runner (find-card "Parasite" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No prompt to host Parasite")
      (is (= 1 (count (get-runner-facedown state))) "Parasite installed face down")))
  (testing "Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (let [arch (get-ice state :hq 0)]
        (core/rez state :corp arch)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner arch)
        (let [psite (first (:hosted (refresh arch)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 4 (get-counters (refresh psite) :virus)) "Parasite has 4 counters")
          (is (= -1 (:current-strength (refresh arch))) "Architect at -1 strength")))))
  (testing "Should stay on hosted card moved by Builder"
    (do-game
      (new-game {:corp {:deck [(qty "Builder" 3) "Ice Wall"]}
                 :runner {:deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Builder" "Archives")
      (let [builder (get-ice state :archives 0)]
        (core/rez state :corp builder)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner))
        (let [orig-builder (refresh builder)]
          (card-ability state :corp builder 0)
          (click-prompt state :corp "HQ")
          (let [moved-builder (get-ice state :hq 1)]
            (is (= (:current-strength orig-builder) (:current-strength moved-builder)) "Builder's state is maintained")
            (let [orig-psite (dissoc (first (:hosted orig-builder)) :host)
                  moved-psite (dissoc (first (:hosted moved-builder)) :host)]
              (is (= orig-psite moved-psite) "Hosted Parasite is maintained"))
            (take-credits state :corp)
            (let [updated-builder (refresh moved-builder)
                  updated-psite (first (:hosted updated-builder))]
              (is (= 2 (:current-strength updated-builder)) "Builder strength still reduced")
              (is (= 2 (get-counters (refresh updated-psite) :virus)) "Parasite counters still incremented")))))))
  (testing "Use Hivemind counters when installed; instantly trash ICE if counters >= ICE strength"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck ["Parasite"
                                 "Grimoire"
                                 "Hivemind"
                                 "Sure Gamble"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
          (play-from-hand state :runner "Parasite")
          (click-card state :runner enig)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed instantly")
          (is (= 4 (core/available-mu state)))
          (is (= 2 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed")))))
  (testing "Trashed along with host ICE when its strength has been reduced to 0"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3) (qty "Hedge Fund" 3)]}
                 :runner {:deck [(qty "Parasite" 3) "Grimoire"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (click-card state :runner enig)
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed")
          (is (= 1 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed"))))))

(deftest pelangi
  ;; Pelangi
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Pelangi"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Pelangi")
    (let [iw (get-ice state :hq 0)
          pelangi (get-program state 0)]
      (run-on state "HQ")
      (card-ability state :runner pelangi 0)
      (core/rez state :corp iw)
      (card-ability state :runner pelangi 0)
      (click-prompt state :runner "Code Gate")
      (is (utils/has? (refresh iw) :subtype "Code Gate") "Ice Wall gained Code Gate")
      (run-continue state)
      (is (not (utils/has? (refresh iw) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest peregrine
  ;; Peregrine - 2c to return to grip and derez an encountered code gate
  (do-game
    (new-game {:corp {:deck ["Paper Wall" (qty "Bandwidth" 2)]}
               :runner {:deck ["Peregrine"]}})
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Bandwidth" "Archives")
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Peregrine")
    (let [bw1 (get-ice state :archives 0)
          pw (get-ice state :archives 2)
          per (get-program state 0)]
      (run-on state "Archives")
      (core/rez state :corp pw)
      (core/rez state :corp bw1)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on a barrier")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (and (= 2 (:credit (get-runner))) (empty? (:hand (get-runner)))) "Can't use Peregrine on unrezzed code gate")
      (run-continue state)
      (card-ability state :runner per 2)
      (is (zero? (:credit (get-runner))) "Spent 2 credits")
      (is (= 1 (count (:hand (get-runner)))) "Peregrine returned to grip")
      (is (not (:rezzed (refresh bw1))) "Bandwidth derezzed"))))

(deftest persephone
  ;; Persephone's ability trashes cards from R&D, triggering AR-Enhanced Security
  ;; See #3187
  (do-game
    (new-game {:corp {:deck ["Zed 1.0" (qty "Zed 2.0" 3) "AR-Enhanced Security"]}
               :runner {:deck [(qty "Persephone" 10)]}})
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "AR-Enhanced Security" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Zed 1.0" "Archives")
    (core/rez state :corp (get-ice state :archives 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Persephone")
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 1 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")
    (take-credits state :runner)
    ;; Gotta move the discarded cards back to the deck
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (core/move state :corp (find-card "Zed 2.0" (:discard (get-corp))) :deck)
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "2")
    (is (= 2 (count-tags state)) "Runner took 1 tag from using Persephone's ability while AR-Enhanced Security is scored")))

(deftest pheromones
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
    (new-game {:runner {:deck ["Pheromones"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Pheromones")
    (let [ph (get-program state 0)]
      (card-ability state :runner (refresh ph) 0)
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
      (card-ability state :runner (refresh ph) 0)))) ; this doesn't do anything, but shouldn't crash

(deftest plague
  ;; Plague
  (do-game
    (new-game {:corp {:deck ["Mark Yale"]}
               :runner {:deck ["Plague"]}})
    (play-from-hand state :corp "Mark Yale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Plague")
    (click-prompt state :runner "Server 1")
    (let [plague (get-program state 0)]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor
  ;; Progenitor
  (testing "Hosting Hivemind, using Virus Breeding Ground. Issue #738"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Virus Breeding Ground" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (play-from-hand state :runner "Virus Breeding Ground")
      (is (= 4 (core/available-mu state)))
      (let [prog (get-program state 0)
            vbg (get-resource state 0)]
        (card-ability state :runner prog 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 4 (core/available-mu state)) "No memory used to host on Progenitor")
        (let [hive (first (:hosted (refresh prog)))]
          (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
          (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
          (is (zero? (:credit (get-runner))) "Full cost to host on Progenitor")
          (take-credits state :runner 1)
          (take-credits state :corp)
          (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
          (click-card state :runner hive)
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
          (is (zero? (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Progenitor" "Hivemind"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Progenitor")
      (let [pro (get-program state 0)]
        (card-ability state :runner pro 0)
        (click-card state :runner (find-card "Hivemind" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not deducted from available MU")
        ;; Trash Hivemind
        (core/move state :runner (find-card "Hivemind" (:hosted (refresh pro))) :discard)
        (is (= 4 (core/available-mu state)) "Hivemind 2 MU not added to available MU")))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Reaver" (qty "Fall Guy" 5)]}})
      (starting-hand state :runner ["Reaver" "Fall Guy"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Reaver")
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash") ; Trash PAD campaign
      (is (= 2 (count (:hand (get-runner)))) "Drew a card from trash of corp card")
      (play-from-hand state :runner "Fall Guy")
      (play-from-hand state :runner "Fall Guy")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      ; No draw from Fall Guy trash as Reaver already fired this turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (zero? (count (:hand (get-runner)))) "No cards in hand")
      (take-credits state :runner)
      ; Draw from Fall Guy trash on corp turn
      (card-ability state :runner (get-resource state 0) 1)
      (is (= 1 (count (:hand (get-runner)))) "One card in hand")))
  (testing "with Freelance Coding Construct - should not draw when trash from hand #2671"
    (do-game
      (new-game {:runner {:deck [(qty "Reaver" 9) "Imp" "Snitch" "Freelance Coding Contract"]}})
      (starting-hand state :runner ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
      (take-credits state :corp)
      (play-from-hand state :runner "Reaver")
      (is (= 3 (count (:hand (get-runner)))) "Four cards in hand")
      (is (= 3 (:credit (get-runner))) "3 credits")
      (play-from-hand state :runner "Freelance Coding Contract")
      (click-card state :runner "Snitch")
      (click-card state :runner "Imp")
      (click-prompt state :runner "Done")
      (is (= 7 (:credit (get-runner))) "7 credits - FCC fired")
      (is (zero? (count (:hand (get-runner)))) "No cards in hand"))))

(deftest rezeki
  ;; Rezeki - gain 1c when turn begins
  (do-game
    (new-game {:runner {:deck ["Rezeki"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rezeki")
    (take-credits state :runner)
    (let [credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (:credit (get-runner)) (+ credits 1)) "Gain 1 from Rezeki"))))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (testing "Basic behaviour - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 5) "Hedge Fund"]}
                 :runner {:deck ["RNG Key" (qty "Paperclip" 2)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (starting-hand state :runner ["RNG Key"])
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "5")
        (click-prompt state :runner "Gain 3 [Credits]")
        (is (= 8 (:credit (get-runner))) "Gained 3 credits")
        (click-prompt state :runner "No action"))
      (testing "Do not trigger on second successful run"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No action")
        (take-credits state :runner)
        (take-credits state :corp))
      (testing "Do not trigger on archives"
        (run-on state "Archives")
        (run-successful state))
      (testing "Do not get choice if trigger declined"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "No")
        (click-prompt state :runner "No action"))
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Do not gain credits / cards if guess incorrect"
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "No action"))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Gain 2 cards"
        (is (zero? (count (:hand (get-runner)))) "Started with 0 cards")
        (run-on state "R&D")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "3")
        (click-prompt state :runner "Draw 2 cards")
        (click-prompt state :runner "No action")
        (is (= 2 (count (:hand (get-runner)))) "Gained 2 cards")
        (is (zero? (count (:deck (get-runner)))) "Cards came from stack"))))
  (testing "Do not pay out if accessing an upgrade first -- regression test for #3150"
    (do-game
      (new-game {:corp {:deck ["Hokusai Grid" "Hedge Fund"]}
                 :runner {:deck ["RNG Key"]}})
      (play-from-hand state :corp "Hokusai Grid" "HQ")
      (take-credits state :corp)
      (testing "Gain 3 credits"
        (play-from-hand state :runner "RNG Key")
        (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
        (run-on state "HQ")
        (run-successful state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "2")
        (click-prompt state :runner "Unrezzed upgrade in HQ")
        (is (= "You accessed Hokusai Grid." (-> (get-runner) :prompt first :msg))
            "No RNG Key prompt, straight to access prompt")
        (is (= 5 (:credit (get-runner))) "Gained no credits")))))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game {:runner {:deck ["Scheherazade" "Cache"
                               "Inti" "Fall Guy"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [sch (get-program state 0)]
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Inti" (:hand (get-runner))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-runner))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (is (= 3 (core/available-mu state)) "Programs hosted on Scheh consume MU")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (card-ability state :runner sch 0)
      (click-card state :runner (find-card "Fall Guy" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-runner))))))))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a program and install it. Shuffle.
  (do-game
    (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :runner :credit 5)
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)]
      (card-ability state :runner smc1 0)
      (click-prompt state :runner (find-card "Reaver" (:deck (get-runner))))
      (is (= 6 (:credit (get-runner))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (core/available-mu state)) "SMC MU refunded")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner smc2 0)
      (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
      (is (zero? (count (:deck (get-runner)))) "Deck empty"))))

(deftest shiv
  ;; Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link
  (do-game
    (new-game {:runner {:id "Nasir Meidan: Cyber Explorer"
                        :deck ["Shiv" (qty "Inti" 2)
                               "Access to Globalsec"]}})
    (is (= 1 (:link (get-runner))) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (core/available-mu state)) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link")
      (is (= 2 (core/available-mu state)) "Shiv stops using MU when 2+ link"))))

(deftest sneakdoor-beta
  (testing "Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits. Issue #1138."
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Gabriel Santiago: Consummate Professional"
                          :deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-program state 0)
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner sb 0)
        (run-successful state)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 3 (:credit (get-runner))) "Gained 2 credits from Gabe's ability")
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
        (is (= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ recorded"))))
  (testing "do not switch to HQ if Archives has Crisium Grid. Issue #1229."
    (do-game
      (new-game {:corp {:deck ["Crisium Grid" "Priority Requisition" "Private Security Force"]}
                 :runner {:deck ["Sneakdoor Beta"]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (trash-from-hand state :corp "Priority Requisition")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [sb (get-program state 0)
            cr (get-content state :archives 0)]
        (core/rez state :corp cr)
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= :archives (get-in @state [:run :server 0])) "Crisium Grid stopped Sneakdoor Beta from switching to HQ"))))
  (testing "Allow Nerve Agent to gain counters. Issue #1158/#955"
    (do-game
      (new-game {:runner {:deck ["Sneakdoor Beta" "Nerve Agent"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Nerve Agent")
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [nerve (get-program state 0)
            sb (get-program state 1)]
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 1 (get-counters (refresh nerve) :virus)))
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 2 (get-counters (refresh nerve) :virus))))))
  (testing "Grant Security Testing credits on HQ."
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Sneakdoor Beta"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))))
      (take-credits state :corp)
      (let [sb (get-program state 0)]
        (click-prompt state :runner "HQ")
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
        (is (= 5 (:credit (get-runner))) "Sneakdoor switched to HQ and earned Security Testing credits")))))

(deftest snitch
  ;; Snitch - Only works on unrezzed ice
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 2)]}
               :runner {:deck ["Snitch"]}})
    (play-from-hand state :corp "Quandary" "R&D")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [hqice (get-ice state :hq 0)]
      (core/rez state :corp hqice))
    (take-credits state :corp)
    (play-from-hand state :runner "Snitch")
    (let [snitch (get-program state 0)]
      ;; unrezzed ice scenario
      (run-on state "R&D")
      (card-ability state :runner snitch 0)
      (is (prompt-is-card? state :runner snitch) "Option to jack out")
      (click-prompt state :runner "Yes")
      ;; rezzed ice scenario
      (run-on state "HQ")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out")
      ;; no ice scenario
      (run-on state "Archives")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out"))))

(deftest snowball
  ;; Snowball - Strength boost until end of run when used to break a subroutine
  (do-game
    (new-game {:corp {:deck ["Spiderweb" "Fire Wall" "Hedge Fund"]}
               :runner {:deck ["Snowball"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Fire Wall" "HQ")
    (play-from-hand state :corp "Spiderweb" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Snowball")
    (let [sp (get-ice state :hq 1)
          fw (get-ice state :hq 0)
          snow (get-program state 0)]
      (run-on state "HQ")
      (core/rez state :corp sp)
      (core/rez state :corp fw)
      (card-ability state :runner snow 1) ; match strength
      (is (= 2 (:current-strength (refresh snow))))
      (card-ability state :runner snow 0) ; strength matched, break a sub
      (card-ability state :runner snow 0) ; break a sub
      (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
      (run-continue state)
      (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
      (card-ability state :runner snow 1)
      (card-ability state :runner snow 1) ; match strength
      (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
      (card-ability state :runner snow 0) ; strength matched, break a sub
      (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
      (run-continue state)
      (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest stargate
  ;; Stargate - once per turn Keyhole which doesn't shuffle
  (testing "Basic test"
    (do-game
     (new-game {:corp {:deck ["Herald" "Troll" (qty "Ice Wall" 10)]}
                :runner {:deck ["Stargate"]}})
     (starting-hand state :corp ["Herald" "Troll"])
     (core/move state :corp (find-card "Herald" (:hand (get-corp))) :deck {:front true})
     (core/move state :corp (find-card "Troll" (:hand (get-corp))) :deck {:front true})
     (is (= "Troll" (-> (get-corp) :deck first :title)) "Troll on top of deck")
     (is (= "Herald" (-> (get-corp) :deck second :title)) "Herald 2nd")
     (take-credits state :corp)
     (play-from-hand state :runner "Stargate")
     (card-ability state :runner (get-program state 0) 0)
     (is (:run @state) "Run initiated")
     (run-successful state)
     (click-prompt state :runner "Troll")
     (is (empty? (:prompt (get-runner))) "Prompt closed")
     (is (not (:run @state)) "Run ended")
     (is (= "Troll" (-> (get-corp) :discard first :title)) "Troll was trashed")
     (is (= "Herald" (-> (get-corp) :deck first :title)) "Herald now on top of R&D")
     (card-ability state :runner (get-program state 0) 0)
     (is (not (:run @state)) "No run ended, as Stargate is once per turn"))))

(deftest study-guide
  ;; Study Guide - 2c to add a power counter; +1 strength per counter
  (do-game
    (new-game {:runner {:deck ["Study Guide" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Study Guide")
    (let [sg (get-program state 0)]
      (card-ability state :runner sg 1)
      (is (= 4 (:credit (get-runner))) "Paid 2c")
      (is (= 1 (get-counters (refresh sg) :power)) "Has 1 power counter")
      (is (= 1 (:current-strength (refresh sg))) "1 strength")
      (card-ability state :runner sg 1)
      (is (= 2 (:credit (get-runner))) "Paid 2c")
      (is (= 2 (get-counters (refresh sg) :power)) "Has 2 power counters")
      (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a Barrier ICE
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Quandary"]}
               :runner {:deck ["Surfer"]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Surfer")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits to install Surfer")
    (core/rez state :corp (get-ice state :hq 1))
    (run-on state "HQ")
    (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
    (let [surf (get-program state 0)]
      (card-ability state :runner surf 0)
      (click-card state :runner (get-ice state :hq 0))
      (is (= 1 (:credit (get-runner))) "Paid 2 credits to use Surfer")
      (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Takobi" "Corroder" "Faust"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Takobi")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Faust")
    (let [tako (get-program state 0)
          corr (get-program state 1)
          faus (get-program state 2)]
      (dotimes [_ 3]
        (card-ability state :runner tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")
      (run-on state "HQ")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt for un-rezzed ice")
      (core/rez state :corp (get-ice state :hq 0))
      (card-ability state :runner tako 1)
      (click-card state :runner (refresh faus))
      (is (not-empty (:prompt (get-runner))) "Can't select AI breakers")
      (click-card state :runner (refresh corr))
      (is (empty? (:prompt (get-runner))) "Can select non-AI breakers")
      (is (= 5 (:current-strength (refresh corr))) "Corroder at +3 strength")
      (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt when too few power counters")
      (core/no-action state :corp nil)
      (run-continue state)
      (is (= 2 (:current-strength (refresh corr))) "Corroder returned to normal strength"))))

(deftest trypano
  (testing "Hivemind and Architect interactions"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 2)]}
                 :runner {:deck [(qty "Trypano" 2) "Hivemind"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-rezzed (get-ice state :hq 0)
            architect-unrezzed (get-ice state :rd 0)]
        (core/rez state :corp architect-rezzed)
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (game.core/get-card state architect-rezzed))
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (= 2 (core/available-mu state)) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [n 4]
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes"))
      (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect is not trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect is not trashed")
      (play-from-hand state :runner "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (zero? (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect was trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect was not trashed")
      (is (= 1 (count (:discard (get-runner)))) "Trypano went to discard")))
  (testing "Fire when Hivemind gains counters"
    (do-game
      (new-game {:corp {:deck ["Architect"]}
                 :runner {:deck ["Trypano" "Hivemind" (qty "Surge" 2)]}})
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-unrezzed (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner architect-unrezzed)
        (is (zero? (count (:discard (get-runner)))) "Trypano not in discard yet")
        (is (= 1 (count (get-ice state :rd))) "Unrezzed Architect is not trashed")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-program state 0)]
          (is (= 1 (get-counters (refresh hive) :virus)) "Hivemind starts with 1 virus counter")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 3 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters")
          (play-from-hand state :runner "Surge")
          (click-card state :runner (refresh hive))
          (is (= 5 (get-counters (refresh hive) :virus)) "Hivemind gains 2 virus counters (now at 5)")
          (is (zero? (count (get-ice state :rd))) "Unrezzed Architect was trashed")
          (is (= 3 (count (:discard (get-runner)))) "Trypano went to discard"))))))

(deftest tycoon
  ;; Tycoon
  (testing "Tycoon gives 2c after using to break ICE"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Tycoon"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Tycoon")
      (let [tycoon (get-program state 0)
            credits (:credit (get-corp))]
        (run-on state "HQ")
        (card-ability state :runner tycoon 0)
        (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
        (run-continue state)
        (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used"))))
  ;; Issue #4220: Tycoon doesn't fire if Corp ends run before ice is passed
  (testing "Tycoon gives 2c even if ICE wasn't passed"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Nisei MK II"]}
                 :runner {:deck ["Tycoon"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (play-and-score state "Nisei MK II")
      (take-credits state :corp)
      (play-from-hand state :runner "Tycoon")
      (let [tycoon (get-program state 0)
            credits (:credit (get-corp))
            nisei (get-scored state :corp 0)]
        (run-on state "HQ")
        (card-ability state :runner tycoon 0)
        (is (= credits (:credit (get-corp))) "Corp doesn't gain credits until encounter is over")
        (card-ability state :corp (refresh nisei) 0)
        (is (= (+ credits 2) (:credit (get-corp))) "Corp gains 2 credits from Tycoon being used after Nisei MK II fires")))))

(deftest upya
  (do-game
    (new-game {:runner {:deck ["Upya"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Upya")
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 3 (get-counters (get-program state 0) :power)) "3 counters on Upya")
    (take-credits state :corp)
    (dotimes [_ 3]
      (run-empty-server state "R&D"))
    (is (= 6 (get-counters (get-program state 0) :power)) "6 counters on Upya")
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 2 (:click (get-runner))) "Gained 2 clicks")
      (card-ability state :runner upya 0)
      (is (= 3 (get-counters (refresh upya) :power)) "Upya not used more than once a turn")
      (is (= 2 (:click (get-runner))) "Still at 2 clicks"))
    (take-credits state :runner)
    (take-credits state :corp)
    (let [upya (get-program state 0)]
      (card-ability state :runner upya 0)
      (is (zero? (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-runner))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wari"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Wari")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Barrier")
    (click-card state :runner (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-runner)))) "Wari in heap")
    (is (seq (get-in @state [:runner :prompt])) "Runner is currently accessing Ice Wall")))

(deftest utae
  ;; Utae
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Utae" (qty "Logic Bomb" 3)]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Utae")
    (let [utae (get-program state 0)]
      (run-on state "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (card-ability state :runner utae 0)
      (click-prompt state :runner "2")
      (card-ability state :runner utae 0)
      (is (empty? (:prompt (get-runner))) "Can only use ability once per run")
      (is (= 1 (:credit (get-runner))))
      (card-ability state :runner utae 1)
      (is (= 1 (:credit (get-runner))) "Cannot use this ability without 3 installed virtual resources")
      (run-jack-out state)
      (core/gain state :runner :click 2)
      (dotimes [_ 3]
        (play-from-hand state :runner "Logic Bomb"))
      (run-on state "HQ")
      (card-ability state :runner utae 1)
      (is (= 0 (:credit (get-runner))) "Able to use ability now"))))

(deftest wyrm
  ;; Wyrm reduces strength of ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-on state "HQ")
    (let [ice-wall (get-ice state :hq 0)
          wyrm (get-program state 0)]
      (core/rez state :corp ice-wall)
      (card-ability state :runner wyrm 1)
      (is (zero? (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
      (card-ability state :runner wyrm 1)
      (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))

(deftest yusuf
  ;; Yusuf gains virus counters on successful runs and can spend virus counters from any installed card
  (testing "Yusuf basic tests"
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Yusuf" "Cache"]}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Yusuf")
      (play-from-hand state :runner "Cache")
      (let [fire-wall (get-ice state :hq 0)
            yusuf (get-program state 0)
            cache (get-program state 1)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh yusuf) :virus)) "Yusuf has 1 virus counter")
        (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
        (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
        (run-on state "HQ")
        (core/rez state :corp fire-wall)
        (card-ability state :runner yusuf 1) ; match strength
        (click-card state :runner cache)
        (click-card state :runner yusuf)
        (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
        (is (= 5 (:current-strength (refresh yusuf))) "Yusuf strength 5")
        (is (zero?(get-counters (refresh yusuf) :virus)) "Yusuf lost 1 virus counter to pump")
        (is (empty? (:prompt (get-runner))) "No prompt open")
        (card-ability state :runner yusuf 0)
        (click-card state :runner cache)
        (click-prompt state :runner "Done")
        (is (= 1 (get-counters (refresh cache) :virus)) "Cache lost its final virus counter"))))
  (testing "Yusuf add strength test"
    (do-game
      (new-game {:corp {:deck ["Fire Wall"]}
                 :runner {:deck ["Yusuf" "Cache"]}})
      (play-from-hand state :corp "Fire Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Yusuf")
      (play-from-hand state :runner "Cache")
      (let [fire-wall (get-ice state :hq 0)
            yusuf (get-program state 0)
            cache (get-program state 1)]
        (run-empty-server state "Archives")
        (is (= 1 (get-counters (refresh yusuf) :virus)) "Yusuf has 1 virus counter")
        (is (= 3 (:current-strength (refresh yusuf))) "Initial Yusuf strength")
        (is (= 3 (get-counters (refresh cache) :virus)) "Initial Cache virus counters")
        (run-on state "HQ")
        (core/rez state :corp fire-wall)
        (card-ability state :runner yusuf 2) ; add strength
        (click-card state :runner cache)
        (click-prompt state :runner "Done")
        (is (= 2 (get-counters (refresh cache) :virus)) "Cache lost 1 virus counter to pump")
        (is (= 4 (:current-strength (refresh yusuf))) "Yusuf strength 4")
        (is (empty? (:prompt (get-runner))) "No prompt open")))))
