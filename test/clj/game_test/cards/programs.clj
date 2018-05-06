(ns game-test.cards.programs
  (:require [game.core :as core]
            [game.utils :refer :all]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards)

(deftest au-revoir
  ;; Au Revoir - Gain 1 credit every time you jack out
  (do-game
    (new-game (default-corp) (default-runner [(qty "Au Revoir" 2)]))
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

(deftest crescentus
  ;; Crescentus should only work on rezzed ice
  (do-game
    (new-game (default-corp [(qty "Quandary" 1)])
              (default-runner [(qty "Crescentus" 1)]))
    (play-from-hand state :corp "Quandary" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Crescentus")
    (run-on state "HQ")
    (let [cres (get-in @state [:runner :rig :program 0])
          q (get-ice state :hq 0)]
      (card-ability state :runner cres 0)
      (is (not (nil? (get-in @state [:runner :rig :program 0]))) "Crescentus could not be used because the ICE is not rezzed")
      (core/rez state :corp q)
      (is (get-in (refresh q) [:rezzed]) "Quandary is now rezzed")
      (card-ability state :runner cres 0)
      (is (nil? (get-in @state [:runner :rig :program 0])) "Crescentus could be used because the ICE is rezzed")
      (is (not (get-in (refresh q) [:rezzed])) "Quandary is no longer rezzed"))))

(deftest datasucker
  ;; Datasucker - Reduce strength of encountered ICE
  (do-game
    (new-game (default-corp [(qty "Fire Wall" 1)])
              (default-runner [(qty "Datasucker" 1)]))
    (play-from-hand state :corp "Fire Wall" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 3)
    (play-from-hand state :runner "Datasucker")
    (let [ds (get-in @state [:runner :rig :program 0])
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

(deftest datasucker-trashed
  ;; Datasucker - does not affect next ice when current is trashed. Issue #1788.
  (do-game
    (new-game
      (default-corp [(qty "Wraparound" 1) (qty "Spiderweb" 1)])
      (default-corp [(qty "Datasucker" 1) (qty "Parasite" 1)]))
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
      (prompt-select :runner (refresh spider))
      (run-on state "HQ")
      (run-continue state)
      (card-ability state :runner (refresh sucker) 0)
      (card-ability state :runner (refresh sucker) 0)
      (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Datasucker")
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Datasucker"))))

(deftest dhegdheer-adept
  ;; Dheghdheer - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Adept" 1)
                               (qty "Dhegdheer" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dhegdheer")
    (play-from-hand state :runner "Adept")
    (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
    (is (= 2 (:memory (get-runner))) "2 MU used")
    (let [dheg (get-program state 0)
          adpt (get-program state 1)]
      (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
      (card-ability state :runner dheg 1)
      (prompt-select :runner (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh dheg)))]
        (is (= 4 (:credit (get-runner))) "4 credits left after hosting")
        (is (= 4 (:memory (get-runner))) "0 MU used")
        (is (= 6 (:current-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))

(deftest diwan
  ;; Diwan - Full test
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Fire Wall" 3) (qty "Crisium Grid" 2)])
              (default-runner [(qty "Diwan" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Diwan")
    (prompt-choice :runner "HQ")
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

(deftest djinn-host-chakana
  ;; Djinn - Hosted Chakana does not disable advancing agendas. Issue #750
  (do-game
    (new-game (default-corp [(qty "Priority Requisition" 1)])
              (default-runner [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (play-from-hand state :corp "Priority Requisition" "New remote")
    (take-credits state :corp 2)
    (play-from-hand state :runner "Djinn")
    (let [djinn (get-in @state [:runner :rig :program 0])
          agenda (get-content state :remote1 0)]
      (is agenda "Agenda was installed")
      (card-ability state :runner djinn 1)
      (prompt-select :runner (find-card "Chakana" (:hand (get-runner))))
      (let [chak (first (:hosted (refresh djinn)))]
        (is (= "Chakana" (:title chak)) "Djinn has a hosted Chakana")
        ;; manually add 3 counters
        (core/add-counter state :runner (first (:hosted (refresh djinn))) :virus 3)
        (take-credits state :runner 2)
        (core/advance state :corp {:card agenda})
        (is (= 1 (:advance-counter (refresh agenda))) "Agenda was advanced")))))

(deftest djinn-host-program
  ;; Djinn - Host a non-icebreaker program
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Djinn" 1) (qty "Chakana" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Djinn")
    (is (= 3 (:memory (get-runner))))
    (let [djinn (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner djinn 1)
      (prompt-select :runner (find-card "Chakana" (:hand (get-runner))))
      (is (= 3 (:memory (get-runner))) "No memory used to host on Djinn")
      (is (= "Chakana" (:title (first (:hosted (refresh djinn))))) "Djinn has a hosted Chakana")
      (is (= 1 (:credit (get-runner))) "Full cost to host on Djinn"))))

(deftest djinn-tutor-virus
  ;; Djinn - Tutor a virus program
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Djinn" 1) (qty "Parasite" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Djinn")
    (core/move state :runner (find-card "Parasite" (:hand (get-runner))) :deck)
    (is (zero? (count (:hand (get-runner)))) "No cards in hand after moving Parasite to deck")
    (let [djinn (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner djinn 0)
      (prompt-card :runner (find-card "Parasite" (:deck (get-runner))))
      (is (= "Parasite" (:title (first (:hand (get-runner))))) "Djinn moved Parasite to hand")
      (is (= 2 (:credit (get-runner))) "1cr to use Djinn ability")
      (is (= 2 (:click (get-runner))) "1click to use Djinn ability"))))

(deftest equivocation
  ;; Equivocation - interactions with other successful-run events.
  (do-game
    (new-game
      (default-corp [(qty "Restructure" 3) (qty "Hedge Fund" 3)])
      (make-deck "Laramy Fisk: Savvy Investor" [(qty "Equivocation" 1) (qty "Desperado" 1)]))
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Equivocation")
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :rd)
    (prompt-choice :runner "Laramy Fisk: Savvy Investor")
    (prompt-choice :runner "Yes")
    (is (= 2 (count (:hand (get-corp)))) "Corp forced to draw by Fisk")
    (prompt-choice :runner "Yes") ; Equivocation prompt
    (prompt-choice :runner "Yes") ; force the draw
    (is (= 1 (:credit (get-runner))) "Runner gained 1cr from Desperado")
    (is (= 3 (count (:hand (get-corp)))) "Corp forced to draw by Equivocation")
    (prompt-choice :runner "No action")
    (is (not (:run @state)) "Run ended")))

(deftest false-echo
  ;; False Echo - choice for Corp
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3)])
              (default-runner [(qty "False Echo" 3)]))
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
      (prompt-choice :corp "Add to HQ")
      (is (= 2 (count (:hand (get-corp)))) "Ice Wall added to HQ")
      (is (= 1 (count (:discard (get-runner)))) "False Echo trashed")
      (run-continue state)
      (card-ability state :runner echo2 0)
      (prompt-choice :corp "Rez")
      (is (:rezzed (get-ice state :archives 0)) "Ice Wall rezzed")
      (is (= 2 (count (:discard (get-runner)))) "False Echo trashed"))))

(deftest gravedigger
  ;; Gravedigger - Gain counters when Corp cards are trashed, spend click-counter to mill Corp
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Enigma" 2)])
              (default-runner [(qty "Gravedigger" 1)]))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Gravedigger")
    (let [gd (get-in @state [:runner :rig :program 0])]
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

(deftest harbinger-blacklist
  ;; Harbinger - install facedown when Blacklist installed
  (do-game
    (new-game (default-corp [(qty "Blacklist" 1)])
              (default-runner [(qty "Harbinger" 1)]))
    (play-from-hand state :corp "Blacklist" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Harbinger")
    (core/trash state :runner (-> (get-runner) :rig :program first))
    (is (= 0 (count (:discard (get-runner)))) "Harbinger not in heap")
    (is (-> (get-runner) :rig :facedown first :facedown) "Harbinger installed facedown")))

(deftest hyperdriver
  ;; Hyperdriver - Remove from game to gain 3 clicks
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Hyperdriver" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Hyperdriver")
    (is (= 1 (:memory (get-runner))) "3 MU used")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [hyp (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner hyp 0)
      (core/end-phase-12 state :runner nil)
      (is (= 7 (:click (get-runner))) "Gained 3 clicks")
      (is (= 1 (count (:rfg (get-runner)))) "Hyperdriver removed from game"))))

(deftest hyperdriver-dhegdheer
  ;; triggering a Dhegdeered Hyperdriver should not grant +3 MU
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Hyperdriver" 1)
                               (qty "Dhegdheer" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Dhegdheer")
    (let [dheg (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner dheg 0)
      (prompt-select :runner (find-card "Hyperdriver" (:hand (get-runner))))
      (is (= 4 (:memory (get-runner))) "0 MU used")
      (is (= 2 (:click (get-runner))) "2 clicks used")
      (is (= 3 (:credit (get-runner))) "2 credits used")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (:runner-phase-12 @state) "Runner in Step 1.2")
      (let [hyp (first (:hosted (refresh dheg)))]
        (card-ability state :runner hyp 0)
        (core/end-phase-12 state :runner nil)
        (is (= 7 (:click (get-runner))) "Used Hyperdriver")
        (is (= 4 (:memory (get-runner))) "Still 0 MU used")))))

(deftest imp
  ;; Imp
  (testing "Full test"
    (letfn [(imp-test [card]
              (do-game
                (new-game (default-corp [(qty card 1)])
                          (default-runner [(qty "Imp" 1)]))
                (take-credits state :corp)
                (play-from-hand state :runner "Imp")
                (run-empty-server state "HQ")
                (prompt-choice-partial :runner "Imp")
                (is (= 1 (count (:discard (get-corp)))))))]
      (doall (map imp-test
                  ["Hostile Takeover"
                   "Dedicated Response Team"
                   "Beanstalk Royalties"
                   "Ice Wall"
                   "Oberth Protocol"]))))
  (testing "vs an ambush"
    (do-game
      (new-game (default-corp [(qty "Prisec" 1)])
                (default-runner [(qty "Imp" 1) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Prisec" "New remote")
      (take-credits state :corp)
      (let [credits (:credit (get-corp))
            tags (:tag (get-runner))
            grip (count (:hand (get-runner)))
            archives (count (:discard (get-corp)))]
        (play-from-hand state :runner "Imp")
        (run-empty-server state :remote1)
        (prompt-choice :corp "Yes")
        (prompt-choice-partial :runner "Imp")
        (is (= 2 (- credits (:credit (get-corp)))) "Corp paid 2 for Prisec")
        (is (= 1 (- (:tag (get-runner)) tags)) "Runner has 1 tag")
        (is (= 2 (- grip (count (:hand (get-runner))))) "Runner took 1 meat damage")
        (is (= 1 (- (count (:discard (get-corp))) archives)) "Used Imp to trash Prisec"))))
  (testing "vs The Future Perfect"
    ;; Psi-game happens on access [5.5.1], Imp is a trash ability [5.5.2]
    (do-game
      (new-game (default-corp [(qty "The Future Perfect" 1)])
                (default-runner [(qty "Imp" 1)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (testing "Access, corp wins psi-game"
        (run-empty-server state "HQ")
        ;; Should access TFP at this point
        (prompt-choice :corp "1 [Credits]")
        (prompt-choice :runner "0 [Credits]")
        (prompt-choice-partial :runner "Imp")
        (take-credits state :runner)
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (= 0 (:agenda-point (get-runner))) "Runner did not steal TFP")
        (core/move state :corp (find-card "The Future Perfect" (:discard (get-corp))) :hand))
      (take-credits state :runner)
      (take-credits state :corp)
      (testing "Access, runner wins psi-game"
        (run-empty-server state "HQ")
        ;; Access prompt for TFP
        (prompt-choice :corp "0 [Credits]")
        (prompt-choice :runner "0 [Credits]")
        ;; Fail psi game
        (prompt-choice-partial :runner "Imp")
        (is (= "The Future Perfect" (get-in @state [:corp :discard 0 :title])) "TFP trashed")
        (is (= 0 (:agenda-point (get-runner))) "Runner did not steal TFP")))))

(deftest incubator-transfer-virus-counters
  ;; Incubator - Gain 1 virus counter per turn; trash to move them to an installed virus program
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Incubator" 1) (qty "Datasucker" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Datasucker")
    (play-from-hand state :runner "Incubator")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [ds (get-in @state [:runner :rig :program 0])
          incub (get-in @state [:runner :rig :program 1])]
      (is (= 1 (get-counters (refresh incub) :virus)) "Incubator gained 1 virus counter")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 2 (get-counters (refresh incub) :virus)) "Incubator has 2 virus counters")
      (card-ability state :runner incub 0)
      (prompt-select :runner ds)
      (is (= 2 (get-counters (refresh ds) :virus)) "Datasucker has 2 virus counters moved from Incubator")
      (is (= 1 (count (get-in @state [:runner :rig :program]))))
      (is (= 1 (count (:discard (get-runner)))) "Incubator trashed")
      (is (= 3 (:click (get-runner)))))))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game (default-corp [(qty "Snowflake" 1)])
              (default-runner [(qty "Ixodidae" 1) (qty "Lamprey" 1)]))
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
      (is (prompt-is-card? :corp s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? :runner s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-corp))) "Corp paid 1 credit to rezz Snowflake")
      (prompt-choice :corp "1")
      (prompt-choice :runner "1")
      (is (= 5 (:credit (get-corp))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-runner))) "Runner did not gain 1 credit from Ixodidae when corp spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-corp))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-runner))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))

(deftest lamprey
  ;; Lamprey - Corp loses 1 credit for each successful HQ run; trashed on purge
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Lamprey" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Lamprey")
    (let [lamp (get-in @state [:runner :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 6 (:credit (get-corp))) "Corp lost 1 credit")
      (run-empty-server state :hq)
      (is (= 5 (:credit (get-corp))) "Corp lost 1 credit")
      (take-credits state :runner)
      (core/purge state :corp)
      (is (empty? (get-in @state [:runner :rig :program])) "Lamprey trashed by purge"))))

(deftest leprechaun-adept
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Adept" 1)
                               (qty "Leprechaun" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Leprechaun")
    (play-from-hand state :runner "Adept")
    (is (= 1 (:memory (get-runner))) "3 MU used")
    (let [lep (get-program state 0)
          adpt (get-program state 1)]
      (is (= 3 (:current-strength (refresh adpt))) "Adept at 3 strength individually")
      (card-ability state :runner lep 1)
      (prompt-select :runner (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh lep)))]
        (is (= 3 (:memory (get-runner))) "1 MU used")
        (is (= 5 (:current-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))

(deftest leprechaun-mu-savings
  ;; Leprechaun - Keep MU the same when hosting or trashing hosted programs
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Leprechaun" 1) (qty "Hyperdriver" 1) (qty "Imp" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Leprechaun")
    (let [lep (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner lep 0)
      (prompt-select :runner (find-card "Hyperdriver" (:hand (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (= 2 (:credit (get-runner))))
      (is (= 3 (:memory (get-runner))) "Hyperdriver 3 MU not deducted from available MU")
      (card-ability state :runner lep 0)
      (prompt-select :runner (find-card "Imp" (:hand (get-runner))))
      (is (= 1 (:click (get-runner))))
      (is (= 0 (:credit (get-runner))))
      (is (= 3 (:memory (get-runner))) "Imp 1 MU not deducted from available MU")
      ;; Trash Hyperdriver
      (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
      (is (= 3 (:memory (get-runner))) "Hyperdriver 3 MU not added to available MU")
      (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
      (is (= 3 (:memory (get-runner))) "Imp 1 MU not added to available MU"))))

(deftest magnum-opus-click
  ;; Magnum Opus - Gain 2 cr
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Magnum Opus" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Magnum Opus")
    (is (= 2 (:memory (get-runner))))
    (is (= 0 (:credit (get-runner))))
    (let [mopus (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner mopus 0)
      (is (= 2 (:credit (get-runner))) "Gain 2cr"))))

(deftest nyashia
  ;; Nyashia
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 10)])
              (default-runner [(qty "Nyashia" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Nyashia")
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :runner "Yes")
    (is (= 2 (+ (get-in @state [:runner :rd-access]) (:access-bonus (:run @state) 0))))))

(deftest origami
  ;; Origami - Increases Runner max hand size
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Origami" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Origami")
    (is (= 6 (core/hand-size state :runner)))
    (play-from-hand state :runner "Origami")
    (is (= 9 (core/hand-size state :runner)) "Max hand size increased by 2 for each copy installed")))

(deftest paintbrush
  ;; Paintbrush - Give rezzed ICE a chosen subtype until the end of the next run
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Paintbrush" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Paintbrush")
    (is (= 2 (:memory (get-runner))))
    (let [iwall (get-ice state :hq 0)
          pb (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner pb 0)
      (prompt-select :runner iwall)
      (is (= 3 (:click (get-runner))) "Ice Wall not rezzed, so no click charged")
      (prompt-choice :runner "Done") ; cancel out
      (core/rez state :corp iwall)
      (card-ability state :runner pb 0)
      (prompt-select :runner iwall)
      (prompt-choice :runner "Code Gate")
      (is (= 2 (:click (get-runner))) "Click charged")
      (is (= true (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall gained Code Gate")
      (run-empty-server state "Archives")
      (is (= false (has? (refresh iwall) :subtype "Code Gate")) "Ice Wall lost Code Gate at the end of the run"))))

(deftest parasite
  (testing "Basic functionality: Gain 1 counter every Runner turn"
    (do-game
      (new-game (default-corp [(qty "Wraparound" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Parasite" 3) (qty "Sure Gamble" 3)]))
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (prompt-select :runner wrap)
        (is (= 3 (:memory (get-runner))) "Parasite consumes 1 MU")
        (let [psite (first (:hosted (refresh wrap)))]
          (is (= 0 (get-counters psite :virus)) "Parasite has no counters yet")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (get-counters (refresh psite) :virus))
              "Parasite gained 1 virus counter at start of Runner turn")
          (is (= 6 (:current-strength (refresh wrap))) "Wraparound reduced to 6 strength")))))
  (testing "Installed facedown w/ Apex"
    (do-game
      (new-game (default-corp)
                (make-deck "Apex: Invasive Predator" [(qty "Parasite" 1)]))
      (take-credits state :corp)
      (core/end-phase-12 state :runner nil)
      (prompt-select :runner (find-card "Parasite" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No prompt to host Parasite")
      (is (= 1 (count (get-in @state [:runner :rig :facedown]))) "Parasite installed face down")))
  (testing "Installed on untrashable Architect should keep gaining counters past 3 and make strength go negative"
    (do-game
      (new-game (default-corp [(qty "Architect" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Parasite" 3) (qty "Grimoire" 1)]))
      (play-from-hand state :corp "Architect" "HQ")
      (let [arch (get-ice state :hq 0)]
        (core/rez state :corp arch)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (prompt-select :runner arch)
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
      (new-game (default-corp [(qty "Builder" 3) (qty "Ice Wall" 1)])
                (default-runner [(qty "Parasite" 3)]))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Builder" "Archives")
      (let [builder (get-ice state :archives 0)]
        (core/rez state :corp builder)
        (take-credits state :corp)
        (play-from-hand state :runner "Parasite")
        (prompt-select :runner builder)
        (let [psite (first (:hosted (refresh builder)))]
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 3 (:current-strength (refresh builder))) "Builder reduced to 3 strength")
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (take-credits state :runner))
        (let [orig-builder (refresh builder)]
          (card-ability state :corp builder 0)
          (prompt-choice :corp "HQ")
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
      (new-game (default-corp [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Parasite" 1)
                                 (qty "Grimoire" 1)
                                 (qty "Hivemind" 1)
                                 (qty "Sure Gamble" 1)]))
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Sure Gamble")
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Hivemind")
        (let [hive (get-in @state [:runner :rig :program 0])]
          (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind has 2 counters")
          (play-from-hand state :runner "Parasite")
          (prompt-select :runner enig)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed instantly")
          (is (= 4 (:memory (get-runner))))
          (is (= 2 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed")))))
  (testing "Trashed along with host ICE when its strength has been reduced to 0"
    (do-game
      (new-game (default-corp [(qty "Enigma" 3) (qty "Hedge Fund" 3)])
                (default-runner [(qty "Parasite" 3) (qty "Grimoire" 1)]))
      (play-from-hand state :corp "Enigma" "HQ")
      (let [enig (get-ice state :hq 0)]
        (core/rez state :corp enig)
        (take-credits state :corp)
        (play-from-hand state :runner "Grimoire")
        (play-from-hand state :runner "Parasite")
        (prompt-select :runner enig)
        (let [psite (first (:hosted (refresh enig)))]
          (is (= 1 (get-counters (refresh psite) :virus)) "Parasite has 1 counter")
          (is (= 1 (:current-strength (refresh enig))) "Enigma reduced to 1 strength")
          (take-credits state :runner)
          (take-credits state :corp)
          (is (= 1 (count (:discard (get-corp)))) "Enigma trashed")
          (is (= 1 (count (:discard (get-runner)))) "Parasite trashed when Enigma was trashed"))))))

(deftest pheromones-no-counters
  ;; Pheromones ability shouldn't have a NullPointerException when fired with 0 virus counter
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Pheromones" 1)]))
    (take-credits state :corp)

    (play-from-hand state :runner "Pheromones")
    (let [ph (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner (refresh ph) 0)
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (= 1 (get-counters (refresh ph) :virus)) "Pheromones gained 1 counter")
      (card-ability state :runner (refresh ph) 0)))) ; this doesn't do anything, but shouldn't crash

(deftest plague
  ;; Plague
  (do-game
    (new-game (default-corp [(qty "Mark Yale" 1)])
              (default-runner [(qty "Plague" 1)]))
    (play-from-hand state :corp "Mark Yale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Plague")
    (prompt-choice :runner "Server 1")
    (let [plague (get-in @state [:runner :rig :program 0])]
      (run-empty-server state "Server 1")
      (is (= 2 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Server 1")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague gained 2 counters")
      (run-empty-server state "Archives")
      (is (= 4 (get-counters (refresh plague) :virus)) "Plague did not gain counters"))))

(deftest progenitor-host-hivemind
  ;; Progenitor - Hosting Hivemind, using Virus Breeding Ground. Issue #738
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Progenitor" 1) (qty "Virus Breeding Ground" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Progenitor")
    (play-from-hand state :runner "Virus Breeding Ground")
    (is (= 4 (:memory (get-runner))))
    (let [prog (get-in @state [:runner :rig :program 0])
          vbg (get-in @state [:runner :rig :resource 0])]
      (card-ability state :runner prog 0)
      (prompt-select :runner (find-card "Hivemind" (:hand (get-runner))))
      (is (= 4 (:memory (get-runner))) "No memory used to host on Progenitor")
      (let [hive (first (:hosted (refresh prog)))]
        (is (= "Hivemind" (:title hive)) "Hivemind is hosted on Progenitor")
        (is (= 1 (get-counters hive :virus)) "Hivemind has 1 counter")
        (is (= 0 (:credit (get-runner))) "Full cost to host on Progenitor")
        (take-credits state :runner 1)
        (take-credits state :corp)
        (card-ability state :runner vbg 0) ; use VBG to transfer 1 token to Hivemind
        (prompt-select :runner hive)
        (is (= 2 (get-counters (refresh hive) :virus)) "Hivemind gained 1 counter")
        (is (= 0 (get-counters (refresh vbg) :virus)) "Virus Breeding Ground lost 1 counter")))))

(deftest progenitor-mu-savings
  ;; Progenitor - Keep MU the same when hosting or trashing hosted programs
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Progenitor" 1) (qty "Hivemind" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Progenitor")
    (let [pro (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner pro 0)
      (prompt-select :runner (find-card "Hivemind" (:hand (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (= 2 (:credit (get-runner))))
      (is (= 4 (:memory (get-runner))) "Hivemind 2 MU not deducted from available MU")
      ;; Trash Hivemind
      (core/move state :runner (find-card "Hivemind" (:hosted (refresh pro))) :discard)
      (is (= 4 (:memory (get-runner))) "Hivemind 2 MU not added to available MU"))))

(deftest reaver
  ;; Reaver - Draw a card the first time you trash an installed card each turn
  (do-game
    (new-game (default-corp [(qty "PAD Campaign" 1)])
              (default-runner [(qty "Reaver" 1) (qty "Fall Guy" 5)]))
    (starting-hand state :runner ["Reaver" "Fall Guy"])
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "Reaver")
    (is (= 1 (count (:hand (get-runner)))) "One card in hand")
    (run-empty-server state "Server 1")
    (prompt-choice-partial :runner "Pay") ; Trash PAD campaign
    (is (= 2 (count (:hand (get-runner)))) "Drew a card from trash of corp card")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Fall Guy")
    (is (= 0 (count (:hand (get-runner)))) "No cards in hand")
    ; No draw from Fall Guy trash as Reaver already fired this turn
    (card-ability state :runner (get-resource state 0) 1)
    (is (= 0 (count (:hand (get-runner)))) "No cards in hand")
    (take-credits state :runner)
    ; Draw from Fall Guy trash on corp turn
    (card-ability state :runner (get-resource state 0) 1)
    (is (= 1 (count (:hand (get-runner)))) "One card in hand")))

(deftest reaver-fcc
  ;; Reaver / Freelance Coding Construct - should not draw when trash from hand #2671
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Reaver" 9) (qty "Imp" 1) (qty "Snitch" 1) (qty "Freelance Coding Contract" 1)]))
    (starting-hand state :runner ["Reaver" "Imp" "Snitch" "Freelance Coding Contract"])
    (take-credits state :corp)
    (play-from-hand state :runner "Reaver")
    (is (= 3 (count (:hand (get-runner)))) "Four cards in hand")
    (is (= 3 (:credit (get-runner))) "3 credits")
    (play-from-hand state :runner "Freelance Coding Contract")
    (prompt-select :runner (find-card "Snitch" (:hand (get-runner))))
    (prompt-select :runner (find-card "Imp" (:hand (get-runner))))
    (prompt-choice :runner "Done")
    (is (= 7 (:credit (get-runner))) "7 credits - FCC fired")
    (is (= 0 (count (:hand (get-runner)))) "No cards in hand")))

(deftest rng-key
  ;; RNG Key - first successful run on RD/HQ, guess a number, gain credits or cards if number matches card cost
  (do-game
    (new-game (default-corp [(qty "Enigma" 5) (qty "Hedge Fund" 1)])
              (default-runner [(qty "RNG Key" 1) (qty "Paperclip" 2)]))
    (starting-hand state :corp ["Hedge Fund"])
    (starting-hand state :runner ["RNG Key"])
    (take-credits state :corp)

    (play-from-hand state :runner "RNG Key")
    (is (= 5 (:credit (get-runner))) "Starts at 5 credits")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner 5)
    (prompt-choice :runner "Gain 3 [Credits]")
    (is (= 8 (:credit (get-runner))) "Gained 3 credits")
    (prompt-choice :runner "No action")

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :runner "No action")
    (take-credits state :runner)
    (take-credits state :corp)

    (run-on state "Archives")
    (run-successful state)
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :runner "No")
    (prompt-choice :runner "No action")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :runner "No action")
    (take-credits state :runner)
    (take-credits state :corp)

    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner 2)
    (prompt-choice :runner "No action")

    (take-credits state :runner)
    (take-credits state :corp)

    (is (= 0 (count (:hand (get-runner)))) "Started with 0 cards")
    (run-on state "R&D")
    (run-successful state)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner 3)
    (prompt-choice :runner "Draw 2 cards")
    (prompt-choice :runner "No action")
    (is (= 2 (count (:hand (get-runner)))) "Gained 2 cards")
    (is (= 0 (count (:deck (get-runner)))) "Cards came from deck")))

(deftest scheherazade
  ;; Scheherazade - Gain 1 credit when it hosts a program
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Scheherazade" 1) (qty "Cache" 1)
                               (qty "Inti" 1) (qty "Fall Guy" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Scheherazade")
    (let [sch (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner sch 0)
      (prompt-select :runner (find-card "Inti" (:hand (get-runner))))
      (is (= 1 (count (:hosted (refresh sch)))))
      (is (= 2 (:click (get-runner))) "Spent 1 click to install and host")
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (is (= 3 (:memory (get-runner))) "Programs hosted on Scheh consume MU")
      (card-ability state :runner sch 0)
      (prompt-select :runner (find-card "Cache" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))))
      (is (= 6 (:credit (get-runner))) "Gained 1 credit")
      (card-ability state :runner sch 0)
      (prompt-select :runner (find-card "Fall Guy" (:hand (get-runner))))
      (is (= 2 (count (:hosted (refresh sch)))) "Can't host non-program")
      (is (= 1 (count (:hand (get-runner))))))))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a program and install it. Shuffle.
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Self-modifying Code" 3) (qty "Reaver" 1)]))
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :runner :credit 5)
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-in @state [:runner :rig :program 0])
          smc2 (get-in @state [:runner :rig :program 1])]
      (card-ability state :runner smc1 0)
      (prompt-card :runner (find-card "Reaver" (:deck (get-runner))))
      (is (= 6 (:credit (get-runner))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (:memory (get-runner))) "SMC MU refunded")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner smc2 0)
      (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
      (is (= 0 (count (:deck (get-runner)))) "Deck empty"))))

(deftest sneakdoor-beta
  (testing "Gabriel Santiago, Ash on HQ should prevent Sneakdoor HQ access but still give Gabe credits. Issue #1138."
    (do-game
      (new-game (default-corp [(qty "Ash 2X3ZB9CY" 1)])
                (make-deck "Gabriel Santiago: Consummate Professional" [(qty "Sneakdoor Beta" 1)]))
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (is (= 1 (:credit (get-runner))) "Sneakdoor cost 4 credits")
      (let [sb (get-in @state [:runner :rig :program 0])
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner sb 0)
        (run-successful state)
        (prompt-choice :corp 0)
        (prompt-choice :runner 0)
        (is (= 3 (:credit (get-runner))) "Gained 2 credits from Gabe's ability")
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)) "Ash interrupted HQ access after Sneakdoor run")
        (is (= :hq (-> (get-runner) :register :successful-run first)) "Successful Run on HQ recorded"))))
  (testing "do not switch to HQ if Archives has Crisium Grid. Issue #1229."
    (do-game
      (new-game (default-corp [(qty "Crisium Grid" 1) (qty "Priority Requisition" 1) (qty "Private Security Force" 1)])
                (default-runner [(qty "Sneakdoor Beta" 1)]))
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
      (new-game (default-corp)
                (default-runner [(qty "Sneakdoor Beta" 1) (qty "Nerve Agent" 1)]))
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Nerve Agent")
      (play-from-hand state :runner "Sneakdoor Beta")
      (let [nerve (get-in @state [:runner :rig :program 0])
            sb (get-in @state [:runner :rig :program 1])]
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 1 (get-counters (refresh nerve) :virus)))
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (= 2 (get-counters (refresh nerve) :virus))))))
  (testing "Grant Security Testing credits on HQ."
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Security Testing" 1) (qty "Sneakdoor Beta" 1)]))
      (take-credits state :corp)
      (play-from-hand state :runner "Sneakdoor Beta")
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (is (= 3 (:credit (get-runner))))
      (take-credits state :corp)
      (let [sb (get-in @state [:runner :rig :program 0])]
        (prompt-choice :runner "HQ")
        (card-ability state :runner sb 0)
        (run-successful state)
        (is (not (:run @state)) "Switched to HQ and ended the run from Security Testing")
        (is (= 5 (:credit (get-runner))) "Sneakdoor switched to HQ and earned Security Testing credits")))))

(deftest snitch
  ;; Snitch - Only works on unrezzed ice
  (do-game
    (new-game (default-corp [(qty "Quandary" 2)])
              (default-runner [(qty "Snitch" 1)]))
    (play-from-hand state :corp "Quandary" "R&D")
    (play-from-hand state :corp "Quandary" "HQ")
    (let [hqice (get-ice state :hq 0)]
      (core/rez state :corp hqice))
    (take-credits state :corp)
    (play-from-hand state :runner "Snitch")
    (let [snitch (get-in @state [:runner :rig :program 0])]
      ;; unrezzed ice scenario
      (run-on state "R&D")
      (card-ability state :runner snitch 0)
      (is (prompt-is-card? :runner snitch) "Option to jack out")
      (prompt-choice :runner "Yes")
      ;; rezzed ice scenario
      (run-on state "HQ")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out")
      ;; no ice scenario
      (run-on state "Archives")
      (card-ability state :runner snitch 0)
      (is (empty? (get-in @state [:runner :prompt])) "No option to jack out"))))

(deftest surfer
  ;; Surfer - Swap position with ice before or after when encountering a Barrier ICE
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1) (qty "Quandary" 1)])
             (default-runner [(qty "Surfer" 1)]))
   (play-from-hand state :corp "Quandary" "HQ")
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Surfer")
   (is (= 3 (:credit (get-runner))) "Paid 2 credits to install Surfer")
   (core/rez state :corp (get-ice state :hq 1))
   (run-on state "HQ")
   (is (= 2 (get-in @state [:run :position])) "Starting run at position 2")
   (let [surf (get-in @state [:runner :rig :program 0])]
     (card-ability state :runner surf 0)
     (prompt-select :runner (get-ice state :hq 0))
     (is (= 1 (:credit (get-runner))) "Paid 2 credits to use Surfer")
     (is (= 1 (get-in @state [:run :position])) "Now at next position (1)")
     (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall now at position 1"))))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game (default-corp [(qty "Enigma" 1)])
              (default-runner [(qty "Takobi" 1) (qty "Corroder" 1) (qty "Faust" 1)]))
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)

    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Takobi")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Faust")
    (let [tako (get-in @state [:runner :rig :program 0])
          corr (get-in @state [:runner :rig :program 1])
          faus (get-in @state [:runner :rig :program 2])]
      (dotimes [_ 3]
        (card-ability state :runner tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")

      (run-on state "HQ")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt for un-rezzed ice")
      (core/rez state :corp (get-ice state :hq 0))
      (card-ability state :runner tako 1)
      (prompt-select :runner (refresh faus))
      (is (not-empty (:prompt (get-runner))) "Can't select AI breakers")
      (prompt-select :runner (refresh corr))
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
      (new-game (default-corp [(qty "Architect" 2)])
                (default-runner [(qty "Trypano" 2) (qty "Hivemind" 1)]))
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Architect" "R&D")
      (let [architect-rezzed (get-ice state :hq 0)
            architect-unrezzed (get-ice state :rd 0)]
        (core/rez state :corp architect-rezzed)
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (prompt-select :runner (game.core/get-card state architect-rezzed))
        (play-from-hand state :runner "Trypano")
        (prompt-select :runner architect-unrezzed)
        (is (= 2 (:memory (get-runner))) "Trypano consumes 1 MU"))
      ;; wait 4 turns to make both Trypanos have 4 counters on them
      (dotimes [n 4]
        (take-credits state :runner)
        (take-credits state :corp)
        (prompt-choice :runner "Yes")
        (prompt-choice :runner "Yes"))
      (is (= 0 (count (:discard (get-runner)))) "Trypano not in discard yet")
      (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect is not trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect is not trashed")
      (play-from-hand state :runner "Hivemind") ; now Hivemind makes both Trypanos have 5 counters
      (is (= 0 (count (get-in @state [:corp :servers :rd :ices]))) "Unrezzed Archiect was trashed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Rezzed Archiect was not trashed")
      (is (= 1 (count (:discard (get-runner)))) "Trypano went to discard"))))

(deftest upya
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Upya" 1)]))
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
      (is (= 0 (get-counters (refresh upya) :power)) "3 counters spent")
      (is (= 5 (:click (get-runner))) "Gained 2 clicks"))))

(deftest wari
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Wari" 1)]))
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Wari")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Yes")
    (prompt-choice :runner "Barrier")
    (prompt-select :runner (get-ice state :rd 0))
    (is (= 1 (count (:discard (get-runner)))) "Wari in heap")
    (is (not (empty? (get-in @state [:runner :prompt]))) "Runner is currently accessing Ice Wall")))

