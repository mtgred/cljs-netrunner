(ns game-test.cards.identities
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "419-amoral-scammer"}
  FourHundredAndNineTeen-amoral-scammer
  ;; 419
  (testing "basic test: Amoral Scammer - expose first installed card unless corp pays 1 credit"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["PAD Campaign" "The Cleaners" (qty "Pup" 3) "Oaktown Renovation"]}
                 :runner {:id "419: Amoral Scammer"}})
      (is (= 5 (:credit (get-corp))) "Starts with 5 credits")
      (play-from-hand state :corp "Pup" "HQ")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "Yes")
      (is (= 4 (:credit (get-corp))) "Pays 1 credit to not expose card")
      (play-from-hand state :corp "Pup" "HQ")
      (is (empty? (:prompt (get-runner))) "No option on second install")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Archives")
      (click-prompt state :runner "No")
      (is (empty? (:prompt (get-corp))) "No prompt if Runner chooses No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "The Cleaners" "New remote")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "No")
      (is (last-log-contains? state "exposes The Cleaners") "Installed card was exposed")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (is (empty? (:prompt (get-corp))) "Cannot expose faceup agendas")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/lose state :corp :credit (:credit (get-corp)))
      (is (zero? (:credit (get-corp))) "Corp has no credits")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (click-prompt state :runner "Yes")
      (is (empty? (:prompt (get-corp))) "No prompt if Corp has no credits")
      (is (last-log-contains? state "exposes PAD Campaign") "Installed card was exposed")))
  (testing "Verify expose can be blocked"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck ["Underway Grid" "Pup"]}
                 :runner {:id "419: Amoral Scammer"}})
      (play-from-hand state :corp "Underway Grid" "New remote")
      (click-prompt state :runner "No")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Pup" "Server 1")
      (click-prompt state :runner "Yes")
      (let [ug (get-in @state [:corp :servers :remote1 :content 0])]
        (core/rez state :corp ug)
        (click-prompt state :corp "No")
        (is (last-log-contains? state "uses Underway Grid to prevent 1 card from being exposed") "Exposure was prevented"))))
  (testing "Ixodidae shouldn't trigger off 419's ability"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:id "419: Amoral Scammer"
                          :deck ["Ixodidae"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Ixodidae")
      (take-credits state :runner)
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (let [corp-credits (:credit (get-corp))
            runner-credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (click-prompt state :corp "Yes")
        (is (= 1 (- corp-credits (:credit (get-corp)))) "Should lose 1 credit from 419 ability")
        (is (zero? (- runner-credits (:credit (get-runner)))) "Should not gain any credits from Ixodidae")))))

(deftest acme-consulting-the-truth-you-need
  (testing "Tag gain when rezzing outermost ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner no longer encountering outermost ice")))
  (testing "Interaction with Data Ward"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Data Ward" (qty "Hedge Fund" 5)]}})
      (core/gain state :corp :credit 10)
      (play-from-hand state :corp "Data Ward" "Archives")
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "Runner does not encounter an unrezzed ice")
      (core/rez state :corp (get-ice state :archives 0))
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (card-subroutine state :corp (get-ice state :archives 0) 0)
      (is (not (:run @state)) "Run ended by Data Ward")))
  (testing "Tag gain when starting run"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner no longer encountering outermost ice")))
  (testing "Tag loss when derezzing ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (core/derez state :corp (get-ice state :archives 0))
      (is (not (core/is-tagged? @state)) "Runner no longer encountering the derezzed ice")))
  (testing "No tag on empty server"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck ["Vanilla" (qty "Hedge Fund" 5)]}})
      (take-credits state :corp)
      (run-on state :archives)
      (is (not (core/is-tagged? @state)) "No ice to encounter")))
  (testing "No tag when encountering second ice"
    (do-game
      (new-game {:corp {:id "Acme Consulting: The Truth You Need"
                        :deck [(qty "Vanilla" 2) (qty "Hedge Fund" 4)]}})
      (play-from-hand state :corp "Vanilla" "Archives")
      (play-from-hand state :corp "Vanilla" "Archives")
      (core/rez state :corp (get-ice state :archives 0))
      (core/rez state :corp (get-ice state :archives 1))
      (take-credits state :corp)
      (run-on state :archives)
      (is (core/is-tagged? @state) "Runner is tagged when encountering outermost ice")
      (run-continue state)
      (is (not (core/is-tagged? @state)) "Runner is not tagged when encountering second ice"))))

(deftest adam-compulsive-hacker
  ;; Adam
  (testing "Allow runner to choose directives"
    (do-game
      (new-game {:runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (is (= 4 (count (get-in @state [:runner :play-area]))) "All directives are in the runner's play area")
      (is (zero? (count (get-in @state [:runner :hand]))))
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (is (= 3 (count (get-resource state))) "3 directives were installed")
      (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
      (let [nat (find-card "Neutralize All Threats" (get-resource state))
            sf (find-card "Safety First" (get-resource state))
            abr (find-card "Always Be Running" (get-resource state))]
        (is (and nat sf abr) "The chosen directives were installed"))))
  (testing "Directives should not grant Pālanā credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (is (= 5 (:credit (get-corp))) "Pālanā does not gain credit from Adam's starting Directives")))
  (testing "Neutralize All Threats interaction with advanceable traps"
    (do-game
      (new-game {:corp {:deck [(qty "Cerebral Overwriter" 3)]}
                 :runner {:id "Adam: Compulsive Hacker"
                          :deck [(qty "Sure Gamble" 3)]}
                 :options {:dont-start-game true}})
      (click-card state :runner (find-card "Neutralize All Threats" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Safety First" (get-in @state [:runner :play-area])))
      (click-card state :runner (find-card "Always Be Running" (get-in @state [:runner :play-area])))
      (click-prompt state :corp "Keep")
      (click-prompt state :runner "Keep")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Cerebral Overwriter" "New remote")
      (advance state (get-content state :remote1 0) 2)
      (take-credits state :corp)
      (run-empty-server state :remote1)
      (click-prompt state :corp "Yes")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (:brain-damage (get-runner))) "Runner took 2 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "1 card in archives"))))

(deftest akiko-nisei-head-case
  ;; Akiko Nisei
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 2 (core/access-count state :runner :rd-access)) "Should access additional card from ability")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 1 (core/access-count state :runner :rd-access)) "Should only access 1 from missed psi game")))
  (testing "Shiro interaction: second sub should give Akiko 2 accesses"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10) "Shiro"]}
                 :runner {:id "Akiko Nisei: Head Case"
                          :deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Shiro"])
      (play-from-hand state :corp "Shiro" "New remote")
      (let [shiro (get-ice state :remote1 0)]
        (core/rez state :corp shiro)
        (take-credits state :corp)
        (run-on state :remote1)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "0 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 2 (core/access-count state :runner :rd-access)) "Should access additional card from ability")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-on state :remote1)
        (card-subroutine state :corp shiro 1)
        (click-prompt state :corp "1 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (is (= 1 (core/access-count state :runner :rd-access)) "Should only access 1 from missed psi game")))))

(deftest alice-merchant-clan-agitator
  ;; Alice Merchant
  (do-game
    (new-game {:runner {:id "Alice Merchant: Clan Agitator"
                        :deck ["Security Testing"]}})
    ; (trash-from-hand state :corp "Hostile Takeover")
    (take-credits state :corp)
    (play-from-hand state :runner "Security Testing")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Archives")
    (run-empty-server state "Archives")
    (click-prompt state :runner "Alice Merchant: Clan Agitator")
    (click-prompt state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= 1 (-> (get-corp) :discard count)) "Alice ability should trash 1 card from HQ")
    (is (-> (get-corp) :discard first :seen not) "Discarded card should be facedown when access is replaced")))

(deftest andromeda-dispossessed-ristie
  ;; Andromeda - 9 card starting hand, 1 link
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "9 card starting hand after mulligan"
    (do-game
      (new-game {:runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}
                 :options {:mulligan :runner}})
      (is (= 1 (:link (get-runner))) "1 link")
      (is (= 9 (count (:hand (get-runner)))) "9 cards in Andromeda starting hand")))
  (testing "should not grant Palana credits"
    (do-game
      (new-game {:corp {:id "Pālanā Foods: Sustainable Growth"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Andromeda: Dispossessed Ristie"
                          :deck [(qty "Sure Gamble" 3) (qty "Desperado" 3)
                                 (qty "Security Testing" 3) (qty "Bank Job" 3)]}})
      (is (= 5 (:credit (get-corp))) "Palana does not gain credit from Andromeda's starting hand"))))

(deftest apex-invasive-predator
  ;; Apex - Allow facedown install of a second console. Issue #1326
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck [(qty "Heartbeat" 2)]}})
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-prompt state :runner "Done") ; no facedown install on turn 1
    (play-from-hand state :runner "Heartbeat")
    (is (= 1 (count (get-hardware state))))
    (take-credits state :runner)
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (is (= 1 (count (get-runner-facedown state))) "2nd console installed facedown")))

(deftest asa-group-security-through-vigilance
  (testing "Asa Group should not allow installing operations"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group should not allow installing agendas"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "Project Vitruvius" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group did not install Agenda with its ability")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote")))
  (testing "Asa Group ordering correct when playing Mirrormorph"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Shipment from MirrorMorph"
                               "Pup"
                               "Red Herrings"
                               "Marilyn Campaign"
                               "Project Vitruvius"]}})
      (let  [marilyn (find-card "Marilyn Campaign" (:hand (get-corp)))
             pup (find-card "Pup" (:hand (get-corp)))
             herrings (find-card "Red Herrings" (:hand (get-corp)))
             vitruvius (find-card "Project Vitruvius" (:hand (get-corp)))]
        (play-from-hand state :corp "Shipment from MirrorMorph")
        (click-card state :corp marilyn)
        (click-prompt state :corp "New remote")
        (is (= (:cid marilyn) (:cid (get-content state :remote1 0))) "Marilyn is installed as first card")
        (click-card state :corp herrings) ;; This should be the Asa prompt, should be automatically installed in remote1
        (is (= (:cid herrings) (:cid (get-content state :remote1 1))) "Red Herrings is installed in Server 1")
        (click-card state :corp vitruvius)
        (click-prompt state :corp "New remote")
        (click-card state :corp pup)
        (click-prompt state :corp "New remote")
        (is (empty? (:prompt (get-corp))) "No more prompts")
        (is (= 6 (count (:servers (get-corp)))) "There are six servers, including centrals"))))
  (testing "don't allow installation of operations"
    (do-game
      (new-game {:corp {:id "Asa Group: Security Through Vigilance"
                        :deck ["Pup" "BOOM!" "Urban Renewal"]}})
      (play-from-hand state :corp "Pup" "New remote")
      (click-card state :corp (find-card "BOOM!" (:hand (get-corp))))
      (is (empty? (get-content state :remote1)) "Asa Group installed an event in a server")
      (click-card state :corp (find-card "Urban Renewal" (:hand (get-corp))))
      (is (= "Urban Renewal" (:title (get-content state :remote1 0))) "Asa Group can install an asset in a remote"))))

(deftest ayla-bios-rahim-simulant-specialist
  ;; Ayla - choose & use cards for NVRAM
  (do-game
    (new-game {:runner {:id "Ayla \"Bios\" Rahim: Simulant Specialist"
                        :deck ["Sure Gamble" "Desperado"
                               "Security Testing" "Bank Job"
                               "Heartbeat" "Eater"]}
               :options {:dont-start-game true}})
    (is (= 6 (count (get-in @state [:runner :play-area]))) "Deck cards are in play area")
    (is (zero? (count (get-in @state [:runner :hand]))))
    (click-card state :runner (find-card "Sure Gamble" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Desperado" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Bank Job" (get-in @state [:runner :play-area])))
    (click-card state :runner (find-card "Eater" (get-in @state [:runner :play-area])))
    (is (= 4 (count (:hosted (:identity (get-runner))))) "4 cards in NVRAM")
    (is (zero? (count (get-in @state [:runner :play-area]))) "The play area is empty")
    (click-prompt state :corp "Keep")
    (click-prompt state :runner "Keep")
    (take-credits state :corp)
    (is (= 2 (count (get-in @state [:runner :hand]))) "There are 2 cards in the runner's Grip")
    (card-ability state :runner (:identity (get-runner)) 0)
    (click-prompt state :runner (find-card "Bank Job" (:hosted (:identity (get-runner)))))
    (is (= 3 (count (get-in @state [:runner :hand]))) "There are 3 cards in the runner's Grip")))

(deftest cerebral-imaging-infinite-frontiers
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game {:corp {:id "Cerebral Imaging: Infinite Frontiers"
                      :deck [(qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (core/hand-size state :corp)) "Max hand size is 13")))

(deftest chaos-theory-wunderkind
  ;; Chaos Theory, start with +1 MU
  (do-game
    (new-game {:runner {:id "Chaos Theory: Wünderkind"}})
    (is (= 5 (core/available-mu state)) "Chaos Theory starts the game with +1 MU")))

(deftest chronos-protocol-selective-mind-mapping
  ;; Chronos Protocol - Choose Runner discard for first net damage of a turn
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup" (qty "Neural EMP" 2)]}
                 :runner {:deck [(qty "Imp" 3)]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (run-on state :hq)
      (let [pup (get-ice state :hq 0)]
        (core/rez state :corp pup)
        (card-subroutine state :corp pup 0)
        (click-prompt state :corp "Yes")
        (let [imp (find-card "Imp" (:hand (get-runner)))]
          (click-prompt state :corp imp)
          (is (= 1 (count (:discard (get-runner)))))
          (card-subroutine state :corp pup 0)
          (is (empty? (:prompt (get-corp))) "No choice on second net damage")
          (is (= 2 (count (:discard (get-runner)))))
          (run-jack-out state)
          (take-credits state :runner)
          (core/move state :runner (find-card "Imp" (:discard (get-runner))) :hand)
          (play-from-hand state :corp "Neural EMP")
          (click-prompt state :corp "No")
          (is (= 2 (count (:discard (get-runner)))) "Damage dealt after declining ability")
          (play-from-hand state :corp "Neural EMP")
          (is (empty? (:prompt (get-corp))) "No choice after declining on first damage")
          (is (= 3 (count (:discard (get-runner)))))))))
  (testing "with Obokata: Pay 4 net damage to steal.  Only 3 damage left after Chronos.  No trigger of damage prevent."
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck [(qty "Obokata Protocol" 5)]}
                 :runner {:deck [(qty "Sure Gamble" 3) "Inti" "Feedback Filter"]}})
      (core/gain state :runner :credit 10)
      (play-from-hand state :corp "Obokata Protocol" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 net damage to steal")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp (find-card "Inti" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "Feedback Filter net damage prevention opportunity not given")
      (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 net damage")))
  (testing "vs Employee Strike. Issue #1958"
    (do-game
      (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                        :deck ["Pup"]}
                 :runner {:deck ["Employee Strike" (qty "Scrubbed" 3) "Sure Gamble"]}})
      (play-from-hand state :corp "Pup" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (run-on state :hq)
      (let [pup (get-ice state :hq 0)]
        (core/rez state :corp pup)
        (card-subroutine state :corp pup 0)
        (is (empty? (:prompt (get-corp))) "No choice because of Employee Strike")
        (card-subroutine state :corp pup 0)
        (is (= 2 (count (:discard (get-runner)))))
        (run-jack-out state)
        (take-credits state :runner)
        (take-credits state :corp)
        (play-from-hand state :runner "Scrubbed")
        (run-on state :hq)
        (card-subroutine state :corp pup 0)
        (is (not (empty? (:prompt (get-corp)))) "Employee Strike out of play - Ability turned on correctly")))))

(deftest edward-kim-humanity-s-hammer
  ;; Edward Kim
  (testing "Trash first operation accessed each turn, but not if first one was in Archives"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2) "PAD Campaign"]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Eater" (qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "PAD Campaign")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "No operation trashed from HQ; accessed one in Archives first")
      (take-credits state :runner)
      (core/move state :corp (find-card "Hedge Fund" (:discard (get-corp))) :hand)
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "HQ")
      (is (= 2 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first")
      (take-credits state :runner)
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (play-from-hand state :runner "Eater")
      (let [eater (get-program state 0)]
        (run-on state "Archives")
        (card-ability state :runner eater 0) ; pretend to break a sub so no cards in Archives will be accessed
        (run-successful state)
        (is (= 3 (count (:discard (get-corp)))))
        (run-empty-server state "HQ")
        (is (= 4 (count (:discard (get-corp)))) "1 operation trashed from HQ; accessed non-operation in Archives first"))))
  (testing "Do not trigger maw on first Operation access (due to trash)"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 2)]}
                 :runner {:id "Edward Kim: Humanity's Hammer"
                          :deck ["Maw" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Maw")
      (is (zero? (count (:discard (get-corp)))) "No cards in Archives")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Only one card trashed from HQ, by Ed Kim")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "One more card trashed from HQ, by Maw"))))

(deftest exile-streethawk
  ;; Exile
  (testing "Simultaneous-resolution prompt shown for interaction with Customized Secretary"
    (do-game
      (new-game {:runner {:id "Exile: Streethawk"
                          :deck [(qty "Customized Secretary" 3) (qty "Clone Chip" 3)
                                 (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Customized Secretary" "Clone Chip"])
      (trash-from-hand state :runner "Customized Secretary")
      (play-from-hand state :runner "Clone Chip")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-card state :runner (find-card "Customized Secretary" (:discard (get-runner))))
      ;; Make sure the simultaneous-resolution prompt is showing with 2 choices
      (is (= 2 (-> (get-runner) :prompt first :choices count)) "Simultaneous-resolution prompt is showing")
      (click-prompt state :runner "Exile: Streethawk")
      (is (= 1 (count (:hand (get-runner)))) "Exile drew a card"))))

(deftest freedom-khumalo-crypto-anarchist
  ;; Freedom Khumalo - Can spend virus counters from other cards to trash accessed cards with play/rez costs
  (testing "Only works with Assets, ICE, Operations, and Upgrades"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (click-prompt state :runner "[Freedom]: Trash card")
                (click-card state :runner (get-program state 0))
                (click-card state :runner (get-program state 0))
                (is (= 1 (count (:discard (get-corp))))
                    (str "Accessed " card " should have been trashed after selecting two virus counters"))))]
      (doall (map fk-test
                  ["Dedicated Response Team"
                   "Consulting Visit"
                   "Builder"
                   "Research Station"]))))
  (testing "Triggers when play/rez cost less than or equal to number of available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (let [cost (->> (get-corp) :hand first :cost)]
                  (click-prompt state :runner "[Freedom]: Trash card")
                  (when (pos? cost)
                    (dotimes [_ cost]
                      (click-card state :runner (get-program state 0))))
                  (is (= 1 (count (:discard (get-corp))))
                      (str "Accessed " card " should have been trashed after selecting " cost " virus counters")))))]
      (doall (map fk-test
                  ["Beanstalk Royalties"
                   "Aggressive Negotiation"
                   "Consulting Visit"
                   "Door to Door"]))))
  (testing "Doesn't trigger when there aren't enough available virus counters"
    (letfn [(fk-test [card]
              (do-game
                (new-game {:corp {:deck [card]}
                           :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                                    :deck ["Cache"]}})
                (take-credits state :corp)
                (play-from-hand state :runner "Cache")
                (run-empty-server state "HQ")
                (is (= 1 (-> @state :runner :prompt first :choices count)) "Should only have 1 option")
                (is (= "No action" (-> @state :runner :prompt first :choices first)) "Only option should be 'No action'")))]
      (doall (map fk-test
                  ["Archer"
                   "Fire Wall"
                   "Colossus"
                   "Tyrant"]))))
  (testing "Can use multiple programs for virus counter payment"
    (do-game
      (new-game {:corp {:deck ["Dedicated Response Team"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache" "Virus Breeding Ground"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Virus Breeding Ground")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Freedom]: Trash card")
      (click-card state :runner (get-program state 0))
      (click-card state :runner (get-resource state 0))
      (is (= 1 (count (:discard (get-corp))))
          (str "Accessed Dedicated Response Team should have been trashed after selecting 2 virus counters"))))
  (testing "Can use viruses on hosted cards"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Trypano"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (let [iw (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Trypano")
        (click-card state :runner (refresh iw))
        (take-credits state :runner)
        (take-credits state :corp)
        (click-prompt state :runner "Yes")
        (run-empty-server state "HQ")
        (click-prompt state :runner "[Freedom]: Trash card")
        (click-card state :runner (-> (refresh iw) :hosted first)))
      (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded after selecting 1 virus counter")))
  (testing "Doesn't trigger when accessing an Agenda"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]}
                 :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                          :deck ["Cache"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cache")
      (run-empty-server state "HQ")
      (is (= 1 (->> @state :runner :prompt first :choices count)) "Should only have 1 option")
      (is (= "Steal" (-> @state :runner :prompt first :choices first)) "Only option should be 'Steal'")))
(testing "Shows multiple prompts when playing Imp"
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Sure Gamble" "Cache" "Imp"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Imp")
    (run-empty-server state "HQ")
    (is (= 4 (-> @state :runner :prompt first :choices count)) "Should have 4 options: Freedom, Imp, Trash, No action")))
(testing "Should return to access prompts when Done is pressed"
  (do-game
    (new-game {:corp {:deck ["Dedicated Response Team"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (run-empty-server state "HQ")
    (is (= 3 (->> @state :runner :prompt first :choices count)) "Should have 3 choices: Freedom, Trash, No action")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= 3 (-> @state :runner :prompt first :choices count))
        (str "Should go back to access prompts, with 3 choices: Freedom, Trash, No action. "
             "Choices seen: " (-> @state :runner :prompt first :choices)))
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Card should now be properly discarded")))
(testing "Shouldn't grant additional accesses after trashing accessed card. #3423"
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 10)]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (run-empty-server state "R&D")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Accessed Ice Wall should be discarded now")
    (is (not (:run @state)) "Run ended")))
(testing "Shouldn't give Aumakua additional counters on trash. #3479"
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 10)]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Cache" "Aumakua"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Aumakua")
    (run-empty-server state "R&D")
    (is (zero? (get-counters (get-program state 1) :virus)) "Aumakuma shouldn't have any virus counters yet.")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-corp)))) "Ice Wall should be discarded now")
    (is (zero? (get-counters (get-program state 1) :virus)) "Aumakua doesn't gain any virus counters from trash ability.")
    (is (not (:run @state)) "Run ended")))
(testing "interaction with trash-cost-bonuses, and declining ability once initiated"
  (do-game
    (new-game {:corp {:deck ["The Board"]}
               :runner {:id "Freedom Khumalo: Crypto-Anarchist"
                        :deck ["Skulljack" "Imp" "Sure Gamble"]}})
    (play-from-hand state :corp "The Board" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 1 (-> (get-runner) :prompt first :choices count)) "Runner doesn't have enough credits to trash")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Imp")
    (core/add-counter state :runner (get-program state 0) :virus 5)
    (play-from-hand state :runner "Skulljack")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "The Board should cost 6 to trash")
    (is (= 3 (-> (get-runner) :prompt first :choices count)) "Runner can use Freedom or Imp to trash")
    (click-prompt state :runner "[Freedom]: Trash card")
    (click-card state :runner (get-program state 0))
    (click-prompt state :runner "Done")
    (is (= 6 (core/trash-cost state :runner (get-content state :remote1 0))) "Skulljack shouldn't trigger a second time")
    (is (= 3 (-> (get-runner) :prompt first :choices count)) "Runner can still use Freedom or Imp the second time around")
    (click-prompt state :runner "[Imp]: Trash card")
    (is (= 2 (:agenda-point (get-runner))) "Runner should trash The Board with Imp and gain 2 agenda points"))))

(deftest gabriel-santiago-consummate-professional
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game {:runner {:id "Gabriel Santiago: Consummate Professional"
                        :deck ["Easy Mark"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))

(deftest gagarin-deep-space-expanding-the-horizon
  ;; Gagarin - pay 1c to access each card in remote
  (do-game
    (new-game {:corp {:id "Gagarin Deep Space: Expanding the Horizon"
                      :deck ["PAD Campaign" "Caprice Nisei"]}})
    (core/lose state :runner :credit 4)
    (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-card state :runner (get-content state :remote1 0))
    (is (zero? (:credit (get-runner))) "Paid 1 credit to access")
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "PAD Campaign") "Accessed card name was logged")
    (run-empty-server state :remote1)
    (click-card state :runner (get-content state :remote1 0))
    (click-prompt state :runner "OK") ; Could not afford message dismissed
    (is (empty? (:prompt (get-runner))) "Runner cannot access so no trash prompt")
    (is (not (last-log-contains? state "PAD Campaign")) "No card name was logged")
    (run-empty-server state :hq)
    (click-prompt state :runner "No action") ; Dismiss trash prompt
    (is (last-log-contains? state "Caprice") "Accessed card name was logged")))

(deftest grndl-power-unleashed
  ;; GRNDL: Power Unleashed - start game with 10 credits and 1 bad pub.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity")))
  (testing "vs Valencia - only 1 bad pub at start"
    (do-game
      (new-game {:corp {:id "GRNDL: Power Unleashed"
                        :deck [(qty "Hedge Fund" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 10 (:credit (get-corp))) "GRNDL starts with 10 credits")
      (is (= 1 (:bad-publicity (get-corp))) "GRNDL starts with 1 bad publicity"))))

(deftest haarpsichord-studios-entertainment-unleashed
  ;; Haarpsichord Studios
  (testing "Prevent stealing more than 1 agenda per turn"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
      (take-credits state :runner)
      (play-from-hand state :corp "15 Minutes" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))
  (testing "Interactions with Employee Strike. Issue #1313"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 5)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
      (play-from-hand state :runner "Scrubbed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Third steal prevented"))))

(deftest haas-bioroid-architects-of-tomorrow
  ;; Architects of Tomorrow - prompt to rez after passing bioroid
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Architects of Tomorrow"
                      :deck [(qty "Eli 1.0" 2) "Pup"]}})
    (core/gain state :corp :credit 3)
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (play-from-hand state :corp "Pup" "Archives")
    (play-from-hand state :corp "Eli 1.0" "HQ")
    (take-credits state :corp)
    (run-on state "Archives")
    (core/rez state :corp (get-ice state :archives 1))
    (run-continue state)
    (core/rez state :corp (get-ice state :archives 0))
    (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rezzing Eli 1.0")
    (run-continue state)
    (click-card state :corp (get-ice state :hq 0))
    (is (= 3 (:credit (get-corp))) "Corp not charged for Architects of Tomorrow rez of Eli 1.0")))

(deftest haas-bioroid-engineering-the-future
  ;; Engineereing the Future
  (testing "interaction with Employee Strike"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Engineering the Future"
                        :deck [(qty "Eli 1.0" 3) "Paywall Implementation"]}
                 :runner {:deck ["Employee Strike"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits at turn end")
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
      (play-from-hand state :corp "Paywall Implementation")
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF"))))

(deftest haas-bioroid-stronger-together
  ;; Stronger Together - +1 strength for Bioroid ice
  (do-game
    (new-game {:corp {:id "Haas-Bioroid: Stronger Together"
                      :deck ["Eli 1.0"]}})
    (play-from-hand state :corp "Eli 1.0" "Archives")
    (let [eli (get-ice state :archives 0)]
      (core/rez state :corp eli)
      (is (= 5 (:current-strength (refresh eli))) "Eli 1.0 at 5 strength"))))

(deftest iain-stirling-retired-spook
  ;; Iain Stirling - Gain 2 credits when behind
  (do-game
    (new-game {:corp {:deck ["Breaking News"]}
               :runner {:id "Iain Stirling: Retired Spook"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (is (= 1 (:agenda-point (get-corp))) "Corp gains 1 agenda point from Breaking News")
      (take-credits state :runner 1)
      (is (= 8 (:credit (get-runner))) "Gained 2 credits from being behind on points"))))

(deftest industrial-genomics-growing-solutions
  ;; Industrial Genomics - Increase trash cost
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck [(qty "PAD Campaign" 3) (qty "Hedge Fund" 3)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (core/rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad)))))))
  (testing "with Product Recall"
    (do-game
      (new-game {:corp {:id "Industrial Genomics: Growing Solutions"
                        :deck ["Product Recall" (qty "PAD Campaign" 3) (qty "Hedge Fund" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "PAD Campaign")
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (let [pad (get-content state :remote1 0)]
        (core/rez state :corp pad)
        (take-credits state :corp)
        (run-empty-server state "Server 1")
        (is (= 8 (core/trash-cost state :runner (refresh pad))))
        (run-jack-out state)
        (take-credits state :runner)
        (play-from-hand state :corp "Product Recall")
        (let [credits (:credit (get-corp))]
          (click-card state :corp pad)
          (is (= (+ credits 8) (:credit (get-corp))) "Gain 8 credits from trashing PAD Campaign"))))))

(deftest jemison-astronautics-sacrifice-audacity-success
  ;; Jemison Astronautics - Place advancements when forfeiting agendas
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Enforcer 1.0" "Hostile Takeover" "Ice Wall" "Global Food Initiative"]}
                 :runner {:deck ["Data Dealer"]}})
      (play-from-hand state :corp "Enforcer 1.0" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (let [enf (get-ice state :hq 0)
            iwall (get-ice state :rd 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Data Dealer")
        (run-empty-server state "Server 1")
        (click-prompt state :runner "Steal")
        (let [dd (get-resource state 0)]
          (card-ability state :runner dd 0)
          (click-card state :runner (get-in (get-runner) [:scored 0]))
          (is (empty? (:prompt (get-corp))) "No Jemison prompt for Runner forfeit")
          (take-credits state :runner)
          (play-from-hand state :corp "Global Food Initiative" "New remote")
          (score-agenda state :corp (get-content state :remote2 0))
          (core/rez state :corp enf)
          (click-card state :corp (get-in (get-corp) [:scored 0]))
          (click-card state :corp iwall)
          (is (= 4 (get-counters (refresh iwall) :advancement)) "Jemison placed 4 advancements")))))
  (testing "24/7 - Armed Intimidation combination"
    ;; Expected result: 24/7 causes Forfeit, Jemison places counters, AI triggers
    (do-game
      (new-game {:corp {:id "Jemison Astronautics: Sacrifice. Audacity. Success."
                        :deck ["Armed Intimidation" "Hostile Takeover"
                               "24/7 News Cycle" "Ice Wall"]}})
      (play-and-score state "Hostile Takeover")
      (is (= 1 (:agenda-point (get-corp))) "Corp has 1 agenda points from Hostile Takeover")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits after scoring Hostile Takeover with play-score")
      (play-and-score state "Armed Intimidation")
      (click-prompt state :runner "Take 2 tags")
      (is (= 3 (:agenda-point (get-corp))) "Corp has 3 agenda points from HT + Armed Intimidation")
      (is (= 2 (:tag (get-runner))) "Runner took 2 tags from AI")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "24/7 News Cycle")
      (click-card state :corp (get-scored state :corp 0)) ; select HT to forfeit
      (let [ice-wall (get-ice state :hq 0)]
        (click-card state :corp ice-wall) ; The Jemison forfeit triggers
        (is (= 2 (get-counters (refresh ice-wall) :advancement)) "Ice Wall has 2 advancement counters from HT forfeit"))
      (click-card state :corp (get-scored state :corp 0)) ; select AI to trigger
      (click-prompt state :runner "Take 2 tags") ; First runner has prompt
      (is (= 4 (:tag (get-runner))) "Runner took 2 more tags from AI -- happens at the end of all the async completion"))))

(deftest jesminder-sareen-girl-behind-the-curtain
  ;; Jesminder Sareen - avoid tags only during a run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["SEA Source" "Data Raven"]}
                 :runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Data Raven" "Archives")
      (take-credits state :corp)
      (let [dr (-> @state :corp :servers :archives :ices first)]
        (core/rez state :corp dr)
        (core/click-run state :runner {:server "Archives"})
        (card-ability state :corp dr 0)
        (is (zero? (:tag (get-runner))) "Jesminder avoided first tag during the run")
        (card-ability state :corp dr 0)
        (is (= 1 (:tag (get-runner))) "Jesminder did not avoid the second tag during the run")
        (core/no-action state :corp nil)
        (core/continue state :runner nil)
        (core/no-action state :corp nil)
        (core/successful-run state :runner nil)
        (run-empty-server state "R&D") ; clear per-run buffer
        (take-credits state :runner)
        (play-from-hand state :corp "SEA Source")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 2 (:tag (get-runner))) "Jesminder did not avoid the tag outside of a run"))))
  (testing "don't avoid John Masanori tag"
    (do-game
      (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                          :deck ["John Masanori"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "John Masanori")
      (run-on state "HQ")
      (core/jack-out state :runner nil)
      (is (= 1 (:tag (get-runner))) "Jesminder did not avoid John Masanori tag"))))

(deftest jinteki-biotech-life-imagined
  ;; Jinteki Biotech
  (testing "Brewery net damage"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Brewery")
      (core/start-turn state :corp nil)
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (= 1 (count (:hand (get-runner)))) "Runner took 2 net damage from Brewery flip")))
  (testing "Greenhouse four advancement tokens"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck ["Braintrust"]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Greenhouse")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (take-credits state :runner)
      (let [bt (get-content state :remote1 0)]
        (is (zero? (get-counters (refresh bt) :advancement)) "No advancement counters on agenda")
        (card-ability state :corp (:identity (get-corp)) 1)
        (click-card state :corp (refresh bt))
        (is (= 4 (get-counters (refresh bt) :advancement)) "Four advancement counters on agenda"))))
  (testing "Tank shuffle Archives into R&D"
    (do-game
      (new-game {:corp {:id "Jinteki Biotech: Life Imagined"
                        :deck [(qty "Hedge Fund" 3)]}
                 :options {:dont-start-turn true}})
      (click-prompt state :corp "The Tank")
      (core/start-turn state :corp nil)
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :runner)
      (is (= 3 (count (:discard (get-corp)))) "Archives started with 3 cards")
      (is (zero? (count (:deck (get-corp)))) "R&D started empty")
      (card-ability state :corp (:identity (get-corp)) 1)
      (is (zero? (count (:discard (get-corp)))) "Archives ended empty")
      (is (= 3 (count (:deck (get-corp)))) "R&D ended with 3 cards"))))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game {:corp {:id "Jinteki: Personal Evolution"
                      :deck [(qty "Braintrust" 6)]}
               :runner {:deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))

(deftest jinteki-potential-unleashed
  ;; Potential Unleashed - when the runner takes at least one net damage, mill 1 from their deck
  (do-game
    (new-game {:corp {:id "Jinteki: Potential Unleashed"
                      :deck ["Philotic Entanglement" "Neural EMP" (qty "Braintrust" 3)]}
               :runner {:deck [(qty "Employee Strike" 10)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-from-hand state :corp "Philotic Entanglement" "New remote")
    (score-agenda state :corp (get-content state :remote3 0))
    (is (= 3 (count (:discard (get-runner)))))
    (play-from-hand state :corp "Neural EMP")
    (is (= 5 (count (:discard (get-runner)))))))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "HQ")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))
  (testing "interaction with Employee Strike. Issue #1313 and #1956."
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (play-from-hand state :runner "Employee Strike")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")
      (play-from-hand state :runner "Scrubbed")
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals"))))

(deftest kate-mac-mccaffrey-digital-tinker
  ;; Kate 'Mac' McCaffrey
  (testing "Install discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (:credit (get-runner))) "Installed Magnum Opus for 4 credits")))
  (testing "No discount for 0 cost"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"
                                 "Self-modifying Code"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (play-from-hand state :runner "Magnum Opus")
      (is (zero? (:credit (get-runner))) "No Kate discount on second program install")))
  (testing "Can afford only with the discount"
    (do-game
      (new-game {:runner {:id "Kate \"Mac\" McCaffrey: Digital Tinker"
                          :deck ["Magnum Opus"]}})
      (take-credits state :corp)
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))))
      (play-from-hand state :runner "Magnum Opus")
      (is (= 1 (count (get-program state))) "Magnum Opus installed")
      (is (zero? (:credit (get-runner))) "Installed Magnum Opus for 4 credits"))))

(deftest ken-express-tenma-disappeared-clone
  ;; Ken 'Express' Tenma - Gain 1 credit when first Run event played
  (do-game
    (new-game {:runner {:id "Ken \"Express\" Tenma: Disappeared Clone"
                        :deck [(qty "Account Siphon" 2)]}})
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 6 (:credit (get-runner))) "Gained 1 credit for first Run event")
    (click-prompt state :runner "Replacement effect")
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= 16 (:credit (get-runner))) "No credit gained for second Run event")))

(deftest khan-savvy-skiptracer
  ;; Khan
  (testing "proper order of events when vs. Caprice"
    (do-game
      (new-game {:corp {:deck ["Eli 1.0" "Caprice Nisei"]}
                 :runner {:id "Khan: Savvy Skiptracer"
                          :deck ["Corroder"]}})
      (play-from-hand state :corp "Eli 1.0" "Archives")
      (play-from-hand state :corp "Caprice Nisei" "Archives")
      (core/rez state :corp (get-content state :archives 0))
      (take-credits state :corp)
      (run-on state "Archives")
      (run-continue state)
      (is (and (empty? (:prompt (get-corp)))
               (= 1 (count (:prompt (get-runner))))
               (= "Khan: Savvy Skiptracer" (-> (get-runner) :prompt first :card :title)))
          "Only Khan prompt showing")
      (click-card state :runner (first (:hand (get-runner))))
      (is (find-card "Corroder" (-> (get-runner) :rig :program)) "Corroder installed")
      (is (= 4 (:credit (get-runner))) "1cr discount from Khan")
      (is (= "Caprice Nisei" (-> (get-runner) :prompt first :card :title)) "Caprice prompt showing")
      (click-prompt state :runner "0 [Credits]")
      (click-prompt state :corp "1 [Credits]")
      (is (not (:run @state)) "Run ended"))))

(deftest laramy-fisk-savvy-investor
  ;; Laramy Fisk
  (testing "installing a Shard should still give option to force Corp draw"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Eli 1.0" 3)]}
                 :runner {:id "Laramy Fisk: Savvy Investor"
                          :deck ["Eden Shard"]}})
      (starting-hand state :corp ["Hedge Fund" "Hedge Fund" "Hedge Fund" "Eli 1.0" "Eli 1.0"])
      (take-credits state :corp)
      (run-on state "R&D")
      (core/no-action state :corp nil)
      ;; at Successful Run stage -- click Eden Shard to install
      (play-from-hand state :runner "Eden Shard")
      (is (= 5 (:credit (get-runner))) "Eden Shard install was free")
      (is (= "Eden Shard" (:title (get-resource state 0))) "Eden Shard installed")
      (is (= "Identity" (-> (get-runner) :prompt first :card :type)) "Fisk prompt showing")
      (click-prompt state :runner "Yes")
      (is (not (:run @state)) "Run ended")
      (is (= 6 (count (:hand (get-corp)))) "Corp forced to draw"))))

(deftest leela-patel-trained-pragmatist
  ;; Leela Patel
  (testing "complicated interaction with mutiple Gang Sign"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"
                               "Hostile Takeover"
                               "Geothermal Fracking"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck [(qty "Gang Sign" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "Geothermal Fracking" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "Gang Sign")
      (take-credits state :runner)
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Leela Patel: Trained Pragmatist")
      (click-card state :runner (get-content state :remote2 0))
      (is (find-card "Hostile Takeover" (:hand (get-corp))) "Hostile Takeover returned to hand")
      (click-prompt state :runner "Gang Sign")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Hostile Takeover stolen with Gang Sign")
      (click-card state :runner (get-content state :remote3 0))
      (is (find-card "Geothermal Fracking" (:hand (get-corp))) "Geothermal Fracking returned to hand")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (find-card "Hostile Takeover" (:scored (get-runner))) "Geothermal Fracking stolen with Gang Sign")
      (click-prompt state :runner "Done")))
  (testing "issues with lingering successful run prompt"
    (do-game
      (new-game {:corp {:id "NBN: Making News"
                        :deck ["Breaking News" "SanSan City Grid"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"}})
      (starting-hand state :corp ["SanSan City Grid"])
      (play-from-hand state :corp "SanSan City Grid" "New remote")
      (take-credits state :corp)
      (run-empty-server state :rd)
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :remote1 0))
      (is (not (:run @state)) "Run is over")))
  (testing "upgrades returned to hand in the middle of a run do not break the run. Issue #2008"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 3) (qty "Project Atlas" 3) "Shock!"]}
                 :runner {:id "Leela Patel: Trained Pragmatist"
                          :deck ["Sure Gamble"]}})
      (starting-hand state :corp ["Crisium Grid" "Crisium Grid" "Crisium Grid" "Project Atlas" "Shock!" "Project Atlas"])
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (trash-from-hand state :corp "Project Atlas")
      (trash-from-hand state :corp "Shock!")
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :hq 0))
      (is (not (get-content state :hq 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :rd 0))
      (is (not (get-content state :rd 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses")
      (run-empty-server state "Archives")
      (click-prompt state :runner "Shock!")
      (click-prompt state :runner "Project Atlas")
      (click-prompt state :runner "Steal")
      (click-card state :runner (get-content state :archives 0))
      (is (not (get-content state :archives 0)) "Upgrade returned to hand")
      (is (not (:run @state)) "Run ended, no more accesses"))))

(deftest liza-talking-thunder-prominent-legislator
  ;; Liza Talking Thunder: Prominent Legislator
  (do-game
    (new-game {:runner {:id "Liza Talking Thunder: Prominent Legislator"
                        :deck [(qty "Sure Gamble" 7)]}})
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (is (= 7 (count (:hand (get-runner)))) "Drew 2 cards from successful run on Archives")
    (is (= 1 (:tag (get-runner))) "Took 1 tag from successful run on Archives")))

(deftest maxx-maximum-punk-rock
  ;; MaxX
  (testing "Basic test"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 "Eater"]}})
      (starting-hand state :runner ["Eater"])
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (is (last-log-contains? state "Wyldside, Wyldside")
          "Maxx did log trashed card names")))
  (testing "with Dummy Box. Check that mills don't trigger trash prevention #3246"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Dummy Box" 30)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (play-from-hand state :runner "Dummy Box")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-runner))) "Dummy Box not fired from mill")))
  (testing "with Wyldside - using Wyldside during Step 1.2 should lose 1 click"
    (do-game
      (new-game {:runner {:id "MaxX: Maximum Punk Rock"
                          :deck [(qty "Wyldside" 3)
                                 (qty "Sure Gamble" 3)
                                 (qty "Infiltration" 3)
                                 (qty "Corroder" 3)
                                 (qty "Eater" 3)]}})
      (take-credits state :corp)
      (is (= 2 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
      (starting-hand state :runner ["Wyldside"])
      (play-from-hand state :runner "Wyldside")
      (take-credits state :runner 3)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits at end of first turn")
      (is (find-card "Wyldside" (get-resource state)) "Wyldside was installed")
      (take-credits state :corp)
      (is (zero? (:click (get-runner))) "Runner has 0 clicks")
      (is (:runner-phase-12 @state) "Runner is in Step 1.2")
      (let [maxx (get-in @state [:runner :identity])
            wyld (find-card "Wyldside" (get-resource state))]
        (card-ability state :runner maxx 0)
        (card-ability state :runner wyld 0)
        (core/end-phase-12 state :runner nil)
        (is (= 4 (count (:discard (get-runner)))) "MaxX discarded 2 cards at start of turn")
        (is (= 3 (:click (get-runner))) "Wyldside caused 1 click to be lost")
        (is (= 3 (count (:hand (get-runner)))) "3 cards drawn total")))))

(deftest mti-mwekundu-life-improved
  ;; Mti Mwekundu: Life Improved - when server is approached, install ice from HQ at the innermost position
  (testing "No ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "Multiple ice"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Enigma" "Ice Wall" "Bloom"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Bloom" "R&D")
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (run-continue state)
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Enigma" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma was installed")
      (is (empty? (:hand (get-corp))) "Enigma removed from HQ")))
  (testing "with Kakugo, passing shouldn't fire net damage twice. #3588"
    (do-game
      (new-game {:corp {:id "Mti Mwekundu: Life Improved"
                        :deck ["Kakugo"]}})
      (take-credits state :corp)
      (run-on state "HQ")
      (is (zero? (get-in @state [:run :position])) "Initial position approaching server")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "Kakugo" (:hand (get-corp))))
      (is (= 1 (get-in @state [:run :position])) "Now approaching new ice")
      (is (= "Kakugo" (:title (get-ice state :hq 0))) "Kakugo was installed")
      (is (empty? (:hand (get-corp))) "Kakugo removed from HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= 1 (-> (get-runner) :discard count)) "Runner should take 1 net damage from Kakugo"))))

(deftest nasir-meidan-cyber-explorer
  ;; Nasir
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [iwall (get-ice state :hq 0)
            nasir (get-in @state [:runner :identity])]
        (core/rez state :corp iwall)
        (is (= 5 (:credit (get-runner))) "Nasir Ability does not trigger automatically")
        (card-ability state :runner nasir 0)
        (is (= 1 (:credit (get-runner))) "Credits at 1 after Nasir ability trigger"))))
  (testing "with Xanadu"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:id "Nasir Meidan: Cyber Explorer"
                          :deck ["Xanadu"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (swap! state assoc-in [:runner :credit] 6)
      (play-from-hand state :runner "Xanadu")
      (run-on state "HQ")
      (let [iwall (get-in @state [:corp :servers :hq :ices 0])
            nasir (get-in @state [:runner :identity])]
        (core/rez state :corp iwall)
        (is (= 3 (:credit (get-runner))) "Pay 3 to install Xanadu")
        (card-ability state :runner nasir 0)
        (is (= 2 (:credit (get-runner))) "Gain 1 more credit due to Xanadu")))))

(deftest nathaniel-gnat-hall-one-of-a-kind
  ;; Nathaniel "Gnat" Hall: One-of-a-Kind
  (do-game
    (new-game {:runner {:id "Nathaniel \"Gnat\" Hall: One-of-a-Kind"
                        :deck [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))) "Did not gain a credit when Gnat is on 3 cards")
    (play-from-hand state :runner "Sure Gamble")
    (take-credits state :runner)
    (let [runner-credits (:credit (get-runner))]
      (take-credits state :corp)
      (is (= (inc runner-credits) (:credit (get-runner)))) "Gained 1 credits when on 2 cards")))

(deftest nbn-controlling-the-message
  ;; NBN: Controlling the Message
  (testing "Trace to tag Runner when first installed Corp card is trashed"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck [(qty "Launch Campaign" 3)]}
                 :runner {:deck ["Forger"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Forger")
      ; trash from HQ first - #2321
      (run-empty-server state "HQ")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (empty? (:prompt (get-runner))) "Forger can't avoid the tag")
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (core/gain state :runner :credit 2)
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (is (empty? (:prompt (get-corp))) "No trace chance on 2nd trashed card of turn")))
  (testing "Interaction with Dedicated Response Team"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Launch Campaign" "Dedicated Response Team"]}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Dedicated Response Team" "New remote")
      (core/rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to trash")
      (click-prompt state :corp "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:tag (get-runner))) "Runner took 1 unpreventable tag")
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from DRT"))))

(deftest new-angeles-sol-your-news
  ;; New Angeles Sol - interaction with runner stealing agendas
  (do-game
    (new-game {:corp {:id "New Angeles Sol: Your News"
                      :deck [(qty "Paywall Implementation" 2) "Breaking News"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (play-from-hand state :corp "Paywall Implementation")
    (take-credits state :corp)
    (is (= 6 (:credit (get-corp))))
    (run-empty-server state :remote1)
    (is (= 7 (:credit (get-corp))) "Corp gained 1cr from successful run")
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "Yes")
    (is (find-card "Paywall Implementation" (:discard (get-corp))) "Paywall trashed before Sol triggers")
    (click-card state :corp (find-card "Paywall Implementation" (:hand (get-corp))))
    (is (not (:run @state)) "Run ended")
    (is (find-card "Paywall Implementation" (:current (get-corp))) "Paywall back in play")))

(deftest next-design-guarding-the-net
  ;; Next Design.  Install up to 3 ICE before game starts, one per server max, and re-draw to 5
  (do-game
    (new-game {:corp {:id "NEXT Design: Guarding the Net"
                      :deck [(qty "Snowflake" 10)]}
               :options {:dont-start-turn true}})
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "HQ")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "R&D")
    (click-card state :corp (find-card "Snowflake" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (is (= 2 (count (:hand (get-corp)))) "Corp should have 2 cards in hand")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (= 5 (count (:hand (get-corp)))) "Corp should start with 5 cards in hand")))

(deftest nisei-division-the-next-generation
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game {:corp {:id "Nisei Division: The Next Generation"
                      :deck [(qty "Snowflake" 2)]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (let [s1 (get-in @state [:corp :servers :hq :ices 0])
          s2 (get-in @state [:corp :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :corp s2)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (core/no-action state :corp nil)
      (core/rez state :corp s1)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s1 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))

(deftest noise-hacker-extraordinaire
  ;; Noise: Hacker Extraordinaire
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "PAD Campaign" 3) (qty "Beanstalk Royalties" 2)]}
               :runner {:id "Noise: Hacker Extraordinaire"
                        :deck ["Datasucker" "Cache" "Sure Gamble" (qty "Clone Chip" 2) (qty "Sharpshooter" 2)]}})
    (starting-hand state :runner ["Datasucker" "Sure Gamble" "Clone Chip" "Clone Chip" "Cache"])
    (is (= 6 (count (:hand (get-corp)))) "Corp should start with 6 cards in hand")
    (is (= 5 (count (:deck (get-corp)))) "Corp deck should contain 5 cards")
    (take-credits state :corp)
    (is (zero? (count (:discard (get-corp)))) "Archives started empty")
    (play-from-hand state :runner "Datasucker")
    (is (= 1 (count (:discard (get-corp)))) "Playing virus should cause card to be trashed from R&D")
    (is (= 4 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 1 (count (:discard (get-corp)))) "Playing non-virus should not cause card to be trashed from R&D")
    (core/click-draw state :runner nil)
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Clone Chip")
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Sharpshooter")
    (take-credits state :runner)
    ;; playing virus via Clone Chip on Corp's turn should trigger Noise ability
    (let [chip (get-hardware state 0)]
      (card-ability state :runner chip 0)
      (click-card state :runner (find-card "Cache" (:discard (get-runner))))
      (let [ds (get-program state 1)]
        (is (not (nil? ds)))
        (is (= (:title ds) "Cache"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing virus via Clone Chip on corp's turn should trigger Noise ability")
    (is (= 2 (count (:deck (get-corp)))) "Card trashed to Archives by Noise should come from R&D")
    ;; playing non-virus via Clone Chip on Corp's turn should NOT trigger Noise ability
    (let [chip-2 (get-hardware state 0)]
      (card-ability state :runner chip-2 0)
      (click-card state :runner (find-card "Sharpshooter" (:discard (get-runner))))
      (let [ss (get-program state 2)]
        (is (not (nil? ss)))
        (is (= (:title ss) "Sharpshooter"))))
    (is (= 2 (count (:discard (get-corp)))) "Playing non-virus via Clone Chip on corp's turn should not trigger Noise ability")))

(deftest null-whistleblower
  ;; Null
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Wraparound" 3)]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [null (get-in @state [:runner :identity])
            wrap1 (get-ice state :hq 0)
            wrap2 (get-ice state :hq 1)]
        (card-ability state :runner null 0)
        (is (empty? (:prompt (get-runner))) "Ability won't work on unrezzed ICE")
        (core/rez state :corp wrap2)
        (card-ability state :runner null 0)
        (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 5 (:current-strength (refresh wrap2))) "Wraparound reduced to 5 strength")
        (run-continue state)
        (core/rez state :corp wrap1)
        (card-ability state :runner null 0)
        (is (empty? (:prompt (get-runner))) "Ability already used this turn")
        (run-jack-out state)
        (is (= 7 (:current-strength (refresh wrap2))) "Outer Wraparound back to 7 strength"))))
  (testing "does not affect next ice when current is trashed. Issue #1788."
    (do-game
      (new-game {:corp {:deck ["Wraparound" "Spiderweb"]}
                 :runner {:id "Null: Whistleblower"
                          :deck [(qty "Parasite" 3)]}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (play-from-hand state :corp "Wraparound" "HQ")
      (take-credits state :corp)
      (core/gain state :corp :credit 10)
      (let [null (get-in @state [:runner :identity])
            spider (get-ice state :hq 0)
            wrap (get-ice state :hq 1)]
        (core/rez state :corp spider)
        (core/rez state :corp wrap)
        (play-from-hand state :runner "Parasite")
        (click-card state :runner (refresh spider))
        (run-on state "HQ")
        (run-continue state)
        (card-ability state :runner null 0)
        (click-card state :runner (first (:hand (get-runner))))
        (is (find-card "Spiderweb" (:discard (get-corp))) "Spiderweb trashed by Parasite + Null")
        (is (= 7 (:current-strength (refresh wrap))) "Wraparound not reduced by Null")))))

(deftest omar-keung-conspiracy-theorist
  ;; Omar Keung
  (testing "Make a successful run on the chosen server once per turn"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= [:hq] (get-in @state [:runner :register :successful-run])))
        (is (= "You accessed Hedge Fund." (-> (get-runner) :prompt first :msg)))
        (click-prompt state :runner "No action")
        (is (= 3 (:click (get-runner))))
        (card-ability state :runner omar 0)
        (is (= 3 (:click (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :rd)
        (is (= [:rd] (get-in @state [:runner :register :successful-run])))
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= [:hq :rd] (get-in @state [:runner :register :successful-run]))))))
  (testing "Ash prevents access, but not successful run"
    (do-game
      (new-game {:corp {:deck ["Ash 2X3ZB9CY"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Ash 2X3ZB9CY" "HQ")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            ash (get-content state :hq 0)]
        (core/rez state :corp ash)
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= (:cid ash) (-> (get-runner) :prompt first :card :cid)))
        (is (= :hq (-> (get-runner) :register :successful-run first))))))
  (testing "Crisium Grid prevents prompt"
    (do-game
      (new-game {:corp {:deck ["Crisium Grid"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Crisium Grid" "Archives")
      (take-credits state :corp)
      (let [omar (get-in @state [:runner :identity])
            cr (get-content state :archives 0)]
        (core/rez state :corp cr)
        (card-ability state :runner omar 0)
        (run-successful state)
        (is (= (:cid cr) (-> (get-runner) :prompt first :card :cid)))
        (is (empty? (-> (get-runner) :register :successful-run)))
        (is (= :archives (get-in @state [:run :server 0]))))))
  (testing "When selecting R&D, ability adds counters to Medium"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Medium"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Medium")
      (let [omar (get-in @state [:runner :identity])
            medium (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "R&D")
        (is (= 1 (get-counters (refresh medium) :virus))))))
  (testing "When selecting HQ, ability adds counters to Nerve Agent"
    (do-game
      (new-game {:runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Nerve Agent"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Nerve Agent")
      (let [omar (get-in @state [:runner :identity])
            nerve (get-program state 0)]
        (card-ability state :runner omar 0)
        (run-successful state)
        (click-prompt state :runner "HQ")
        (is (= 1 (get-counters (refresh nerve) :virus)))))))

(deftest quetzal-free-spirit
  ;; Quetzal
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
               :runner {:id "Quetzal: Free Spirit"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [q (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)
          qdef (core/card-def (get-in @state [:runner :identity]))
          qmsg (get-in qdef [:abilities 0 :msg])]
      (core/rez state :corp iwall)
      (card-ability state :runner q 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (run-jack-out state)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (not (last-log-contains? state qmsg)) "Quetzal ability did not trigger")
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (core/click-credit state :runner nil)
      (run-on state "HQ")
      (card-ability state :runner (refresh q) 0)
      (is (last-log-contains? state qmsg) "Quetzal ability did trigger")
      (core/jack-out state :runner nil))))

(deftest reina-roja-freedom-fighter
  ;; Reina Roja - Increase cost of first rezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 3)]}
               :runner {:id "Reina Roja: Freedom Fighter"}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))))
    (run-on state "R&D")
    (let [quan (get-ice state :rd 0)]
      (core/rez state :corp quan)
      (is (= 5 (:credit (get-corp))) "Rez cost increased by 1"))))

(deftest rielle-kit-peddler-transhuman
  ;; Rielle "Kit" Peddler - Give ICE Code Gate
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:id "Rielle \"Kit\" Peddler: Transhuman"
                        :deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (let [k (get-in @state [:runner :identity])
          iwall (get-ice state :hq 0)]
      (core/rez state :corp iwall)
      (card-ability state :runner k 0)
      (is (core/has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (core/has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate"))))

(deftest saraswati-mnemonics-endless-exploration
  ;; Saraswati Mnemonics
  (do-game
    (new-game {:corp {:id "Saraswati Mnemonics: Endless Exploration"
                      :deck ["Gene Splicer" "House of Knives"]}})
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-card state :corp (find-card "Gene Splicer" (:hand (get-corp))))
    (click-prompt state :corp "New remote")
    (let [splicer (get-content state :remote1 0)]
      (is (= 1 (get-counters (refresh splicer) :advancement)) "1 advancements placed on Gene Splicer")
      (core/rez state :corp (refresh splicer))
      (is (not (:rezzed (refresh splicer))) "Gene Splicer did not rez")
      (take-credits state :corp)
      (take-credits state :runner)
      (core/rez state :corp (refresh splicer))
      (is (:rezzed (refresh splicer)) "Gene Splicer now rezzed")
      (card-ability state :corp (get-in @state [:corp :identity]) 0)
      (click-card state :corp (find-card "House of Knives" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [house (get-content state :remote2 0)]
        (advance state house)
        (advance state house)
        (core/score state :corp (refresh house))
        (is (empty? (:scored (get-corp))) "House of Knives not scored")
        (is (zero? (:agenda-point (get-corp))))
        (take-credits state :corp)
        (take-credits state :runner)
        (core/score state :corp (refresh house))
        (is (= 1 (:agenda-point (get-corp))) "House of Knives was able to be scored")))))

(deftest silhouette-stealth-operative
  ;; Silhouette
  (testing "Expose trigger ability resolves completely before access. Issue #2173"
    (do-game
      (new-game {:corp {:deck ["Psychic Field" (qty "Fetal AI" 10)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :deck ["Feedback Filter" "Inside Job"]}})
      (starting-hand state :corp ["Psychic Field" "Fetal AI"])
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Feedback Filter")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [psychic (get-content state :remote1 0)
            ff (get-hardware state 0)]
        (run-empty-server state :hq)
        (is (:run @state) "On successful run trigger effects")
        (click-card state :runner psychic)
        (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
        (click-prompt state :corp "2 [Credits]")
        (click-prompt state :runner "0 [Credits]")
        (card-ability state :runner ff 0)
        (click-prompt state :runner "Done")
        (is (zero? (:credit (get-runner))) "Runner has no more credits left")
        (is (= 1 (count (:hand (get-runner)))) "Prevented 1 net damage")
        (is (empty? (:discard (get-runner))) "No cards discarded")
        (is (:run @state) "On run access phase")
        (click-prompt state :runner "Done")
        (is (empty? (:hand (get-runner))) "Suffered 1 net damage due to accessing Fetal AI")
        (is (= 1 (count (:discard (get-runner)))) "Discarded 1 card due to net damage")
        (is (:run @state) "Resolving access triggers")
        (click-prompt state :runner "No action")
        (is (zero? (count (:scored (get-runner)))) "Runner has no credits to be able to steal Fetal AI")
        (is (not (:run @state)) "Run has now ended")
        (is (= "Flatline" (:reason @state)) "Win condition reports flatline"))))
  (testing "with Temüjin; broken interaction with other successful-run triggers. Issue #1968"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign" (qty "Hedge Fund" 3) (qty "Restructure" 3) (qty "Beanstalk Royalties" 3)]}
                 :runner {:id "Silhouette: Stealth Operative"
                          :deck ["Temüjin Contract" "Desperado"]}})
      (starting-hand state :corp ["Hedge Fund" "PAD Campaign"])
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "Temüjin Contract")
      (click-card state :runner (get-content state :remote1 0))
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 16 (get-counters (get-resource state 0) :credit)) "16 cr on Temujin")
      (is (= 8 (:credit (get-runner))) "Gained 4cr")
      ;; second run
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= "HQ" (:server-target (get-resource state 0))) "Temujin still targeting HQ")
      (is (= 12 (:credit (get-runner))) "Gained 4cr")
      (is (= 12 (get-counters (get-resource state 0) :credit)) "12 cr on Temujin"))))

(deftest skorpios-defense-systems-persuasive-power
  ; Remove a card from game when it moves to discard once per round
  (do-game
    (new-game {:corp {:id "Skorpios Defense Systems: Persuasive Power"
                      :deck ["Hedge Fund" (qty "Quandary" 4)]}
               :runner {:deck ["The Maker's Eye" "Lucky Find"]}})
    (play-from-hand state :corp "Hedge Fund")
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "Lucky Find")
    (play-from-hand state :runner "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    ; Don't allow a run-event in progress to be targeted #2963
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (filter #(= "The Maker's Eye" (:title %)) (-> (get-corp) :prompt first :choices))) "No Maker's Eye choice")
    (click-prompt state :corp "Cancel")
    (run-successful state)
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "1st quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (click-prompt state :runner "No action")
    (is (not (:run @state)))
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (click-prompt state :corp (find-card "The Maker's Eye" (:discard (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rfg]))) "One card RFGed")
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (empty? (:prompt (get-corp))) "Cannot use Skorpios twice")))

(deftest spark-agency-worldswide-reach
  ;; Spark Agency - Rezzing advertisements
  (do-game
    (new-game {:corp {:id "Spark Agency: Worldswide Reach"
                      :deck [(qty "Launch Campaign" 3)]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [lc1 (get-content state :remote1 0)
          lc2 (get-content state :remote2 0)
          lc3 (get-content state :remote3 0)]
      (core/rez state :corp lc1)
      (is (= 4 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Corp turn)")
      (core/rez state :corp lc3)
      (is (= 4 (:credit (get-runner)))
          "Runner did not lose credit from second Spark rez")
      (take-credits state :corp)
      (run-on state "Server 1")
      (core/rez state :corp lc2)
      (is (= 3 (:credit (get-runner)))
          "Runner lost 1 credit from rez of advertisement (Runner turn)"))))

(deftest sportsmetal-go-big-or-go-home
  ;; SportsMetal - gain 2 credits or draw 2 cards on agenda scored or stolen
  (testing "Gain 2 credits on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "2 credits")
      (is (= 7 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Gain 2 credits on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger"]}})
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 7 (:credit (get-corp))) "Corp starts with 7 credits")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "2 credits")
      (is (= 9 (:credit (get-corp))) "Corp gains 2 credits")))
  (testing "Gain 2 cards on score"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :corp "2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards")))
  (testing "Gain 2 cards on steal"
    (do-game
      (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                        :deck ["Merger" (qty "Hedge Fund" 2)]}})
      (starting-hand state :corp ["Merger"])
      (play-from-hand state :corp "Merger" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (empty? (:hand (get-corp))) "Corp starts with no cards")
      (click-prompt state :runner "Steal")
      (click-prompt state :corp "2 cards")
      (is (= 2 (count (:hand (get-corp)))) "Corp draws 2 cards"))))

(deftest sso-industries-fueling-innovation
  ;; SSO Industries: Fueling Innovation - add advancement tokens on ice for faceup agendas
  (do-game
    (new-game {:corp {:id "SSO Industries: Fueling Innovation"
                      :deck [(qty "Hortum" 2) (qty "Oaktown Renovation" 2) "Braintrust"]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no faceup agenda available")
    (take-credits state :runner)
    (play-from-hand state :corp "Oaktown Renovation" "New remote")
    (take-credits state :corp)
    (is (empty? (:prompt (get-corp))) "Not prompted when no ice available")
    (take-credits state :runner)
    (play-from-hand state :corp "Hortum" "HQ")
    (play-from-hand state :corp "Hortum" "R&D")
    (let [h0 (get-ice state :hq 0)
          h1 (get-ice state :rd 0)]
      (is (zero? (get-counters (refresh h0) :advancement)) "Starts with 0 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Starts with 0 tokens")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h0))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Gains 2 tokens")
      (is (zero? (get-counters (refresh h1) :advancement)) "Stays at 0 tokens")
      (take-credits state :runner)
      (play-from-hand state :corp "Oaktown Renovation" "New remote")
      (take-credits state :corp)
      (click-prompt state :corp "Yes")
      (click-card state :corp (refresh h1))
      (is (= 2 (get-counters (refresh h0) :advancement)) "Stays at 2 tokens")
      (is (= 4 (get-counters (refresh h1) :advancement)) "Gains 4 tokens")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (empty? (:prompt (get-corp))) "Not prompted when all ice advanced"))))

(deftest strategic-innovations-future-forward
  ;; Strategic Innovations: Future Forward
  (do-game
    (new-game {:corp {:id "Strategic Innovations: Future Forward"
                      :deck [(qty "Hedge Fund" 2) (qty "Eli 1.0" 2) (qty "Crick" 2)]}})
    (play-from-hand state :corp "Eli 1.0" "New remote")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Crick" "New remote")
    (let [i1 (get-ice state :remote1 0)
          i2 (get-ice state :remote2 0)]
      (take-credits state :corp 0)
      (take-credits state :runner)
      (core/rez state :corp i1)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 1 (count (:prompt (get-corp)))) "Corp prompted to trigger Strategic Innovations")
      (click-card state :corp (first (:discard (get-corp))))
      (is (empty? (:discard (get-corp))) "Hedge Fund moved back to R&D")
      (take-credits state :corp)
      (core/rez state :corp i2)
      (take-credits state :runner)
      (is (zero? (count (:prompt (get-corp))))
          "Corp not prompted to trigger Strategic Innovations"))))

(deftest the-foundry-refining-the-process
  ;; The Foundry
  (testing "interaction with Accelerated Beta Test"
    (do-game
      (new-game {:corp {:id "The Foundry: Refining the Process"
                        :deck [(qty "Accelerated Beta Test" 2) (qty "Eli 1.0" 3)]}})
      (starting-hand state :corp ["Accelerated Beta Test"])
      (play-from-hand state :corp "Accelerated Beta Test" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Eli 1.0" (:play-area (get-corp))))
      (click-prompt state :corp "Archives")
      (click-prompt state :corp "Yes")
      (is (empty? (:play-area (get-corp))) "Play area shuffled into R&D")
      (is (= 1 (count (:hand (get-corp)))) "Added Eli 1.0 to HQ"))))

(deftest the-outfit-family-owned-and-operated
  ;; The Outfit - Gain 3 whenever you take at least 1 bad publicity
  (testing "basic test"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover" "Profiteering"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= 15 (:credit (get-corp))) "Corp should gain 10 credits")
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote2 0))
      (click-prompt state :corp "3")  ;; Take 3 bad publicity from Profiteering, gain 15
      (is (= 4 (:bad-publicity (get-corp))) "Corp should gain 1 bad publicity")
      (is (= 33 (:credit (get-corp))) "Corp should gain 18 credits")))
  (testing "with Profiteering - Only gain 3 credits when taking more than 1 bad publicity in a single effect"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Profiteering"]}})
      (play-from-hand state :corp "Profiteering" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :corp "3")
      (is (= 3 (:bad-publicity (get-corp))) "Take 3 bad publicity")
      (is (= 23 (:credit (get-corp))) "Gain 15 from Profiteering + 3 from The Outfit")))
  (testing "vs Valencia - 1 bad pub at start means 5 credits to start with (does not _gain_ BP)"
    (do-game
      (new-game {:corp {:id "The Outfit: Family Owned and Operated"
                        :deck ["Hostile Takeover"]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 1 (:bad-publicity (get-corp))) "The Outfit starts with 1 bad publicity")
      (is (= 5 (:credit (get-corp))) "The Outfit starts with 8 credits")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 2 (:bad-publicity (get-corp))) "Take 1 bad publicity")
      (is (= (+ 5 7 3) (:credit (get-corp))) "Gain 7 from Hostile Takeover + 3 from The Outfit"))))

(deftest titan-transnational-investing-in-your-future
  ;; Titan Transnational
  (testing "Add a counter to a scored agenda"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Project Atlas"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (let [atl (get-content state :remote1 0)]
        (core/gain state :corp :click 1)
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/advance state :corp {:card (refresh atl)})
        (core/score state :corp {:card (refresh atl)})
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters scored :agenda)) "1 counter added by Titan")))))
  (testing "only use one counter of Corporate Sales Team"
    (do-game
      (new-game {:corp {:id "Titan Transnational: Investing In Your Future"
                        :deck ["Corporate Sales Team" "Mark Yale"]}})
      (play-from-hand state :corp "Corporate Sales Team" "New remote")
      (play-from-hand state :corp "Mark Yale" "New remote")
      (let [cst (get-content state :remote1 0)
            my (get-content state :remote2 0)]
        (core/gain state :corp :click 3)
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/advance state :corp {:card (refresh cst)})
        (core/score state :corp {:card (refresh cst)})
        (let [scored (get-scored state :corp 0)]
          (is (= 1 (get-counters (refresh scored) :agenda)) "1 counter added by Titan")
          (is (= 10 (get-counters (refresh scored) :credit)) "10 credits from Titan")
          (core/rez state :corp my)
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "Agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale")
          (card-ability state :corp my 1)
          (click-card state :corp (refresh scored))
          (is (zero? (get-counters (refresh scored) :agenda)) "No agenda counter used by Mark Yale")
          (is (= 10 (get-counters (refresh scored) :credit)) "Credits not used by Mark Yale"))))))

(deftest weyland-consortium-builder-of-nations
  ;; Builder of Nations
  (testing "1 meat damage per turn at most"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "Hedge Fund" 3)]}})
      (let [bon (get-in @state [:corp :identity])]
        (card-ability state :corp bon 0)
        (click-prompt state :corp "Cancel")
        (is (zero? (count (:discard (get-runner)))) "Runner took no meat damage from BoN")
        (card-ability state :corp bon 0)
        (click-prompt state :corp "Yes")
        (is (= 1 (count (:discard (get-runner)))) "Runner took 1 meat damage from BoN")
        (card-ability state :corp bon 0)
        (is (= 1 (count (:discard (get-runner)))) "Runner took only 1 meat damage from BoN total")
        (is (zero? (count (:prompt (get-corp))))))))
  (testing "2 meat damage from ID ability when The Cleaners is scored"
    (do-game
      (new-game {:corp {:id "Weyland Consortium: Builder of Nations"
                        :deck [(qty "The Cleaners" 3) (qty "Ice Wall" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 2)]}})
      (play-from-hand state :corp "The Cleaners" "New remote")
      (let [clean (get-content state :remote1 0)]
        (score-agenda state :corp clean)
        (let [bon (get-in @state [:corp :identity])]
          (card-ability state :corp bon 0)
          (click-prompt state :corp "Yes")
          (is (= 2 (count (:discard (get-runner)))) "Runner took 2 meat damage from BoN/Cleaners combo"))))))

(deftest whizzard-master-gamer
  ;; Whizzard - Recurring credits
  (do-game
    (new-game {:runner {:id "Whizzard: Master Gamer"
                        :deck ["Sure Gamble"]}})
    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :runner (:identity (get-runner)) 0)))]
      (is (changes-credits (get-runner) 1 (click-whizzard 1)))
      (is (changes-credits (get-runner) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")
      (take-credits state :corp)
      (is (changes-credits (get-runner) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")
      (take-credits state :runner)
      (is (changes-credits (get-runner) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))

(deftest wyvern-chemically-enhanced
  ;; Wyvern: Chemically Enhanced
  (do-game
    (new-game {:corp {:deck [(qty "Launch Campaign" 3)]}
               :runner {:id "Wyvern: Chemically Enhanced"
                        :deck [(qty "Sure Gamble" 2) "Corroder"
                               "Clone Chip" "Easy Mark"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Easy Mark")
    (play-from-hand state :runner "Corroder")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Pay 2 [Credits] to trash")  ;; trash Launch Campaign, should trigger wyvern
    (is (= "Sure Gamble" (:title (last (:discard (get-runner)))))
        "Sure Gamble still in Wyvern's discard")
    (is (some #(= "Easy Mark" (:title %)) (:deck (get-runner))) "Easy Mark moved to deck")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Pay 2 [Credits] to trash")
    (is (= "Sure Gamble" (:title (last (:discard (get-runner))))) "Sure Gamble still in Wyvern's discard")))
