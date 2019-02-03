(ns game.cards.programs
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer [str->int other-side is-tagged? has-subtype?]]
            [jinteki.cards :refer [all-cards]]))

(def card-definitions
  {"Algernon"
   (let [ability {:msg "gain [Click]"
                  :req (req (:runner-phase-12 @state))
                  :cost [:credit 2]
                  :effect (effect (gain :click 1)
                                  (update! (assoc-in (get-card state card) [:special :used-algernon] true)))}]
     {:flags {:runner-phase-12 (req (and (not (can-pay? state :runner nil [:credit 2]))
                                         (some #(card-flag? % :drip-economy true) (all-active-installed state :runner))))}
      :abilities [ability]
      :events
      {:runner-turn-begins
       {:interactive (req (some #(card-flag? % :drip-economy true) (all-active-installed state :runner)))
        :req (req (can-pay? state :runner nil [:credit 2]))
        :optional
        {:prompt "Pay 2 [Credits] to gain [Click]"
         :yes-ability ability}}
       :runner-turn-ends
       {:async true
        :effect (req (if (get-in card [:special :used-algernon])
                       (do
                         (update! state :runner (dissoc-in card [:special :used-algernon]))
                         (if-not (:successful-run runner-reg)
                           (do
                             (system-msg state :runner "trashes Algernon because a successful run did not occur")
                             (trash state :runner eid card nil))
                           (effect-completed state side eid)))
                       (effect-completed state side eid)))}}})

   "Analog Dreamers"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect
                           (run :rd
                                {:req (req (= target :rd))
                                 :replace-access
                                 {:prompt "Choose a card to shuffle into R&D"
                                  :choices {:req #(and (not (ice? %))
                                                       (not (rezzed? %))
                                                       (zero? (get-counters % :advancement)))}
                                  :effect (req (move state :corp target :deck)
                                               (shuffle! state :corp :deck)
                                               (swap! state update-in [:runner :prompt] rest)
                                               (handle-end-run state side)) ; remove the replace-access prompt
                                  :msg "shuffle a card into R&D"}}
                                card))}]}

   "Au Revoir"
   {:events {:jack-out {:effect (effect (gain-credits 1))
                        :msg "gain 1 [Credits]"}}}

   "Bankroll"
   {:implementation "Bankroll gains credits automatically."
    :events {:successful-run {:effect (effect (add-counter card :credit 1)
                                              (system-msg "places 1 [Credit] on Bankroll"))}}
    :abilities [{:label "[Trash]: Take all credits from Bankroll"
                 :async true
                 ;; Cannot trash unless there are counters (so game state changes)
                 :req (req (pos? (get-counters card :credit)))
                 :effect (req (let [credits-on-bankroll (get-counters card :credit)]
                                (wait-for (trash state :runner card {:cause :ability-cost})
                                          (gain-credits state :runner credits-on-bankroll)
                                          (system-msg state :runner (str "trashes Bankroll and takes "
                                                                         credits-on-bankroll " credits from it")))))}]}

   "Bishop"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [b (get-card state card)
                                    hosted? (ice? (:host b))
                                    remote? (is-remote? (second (:zone (:host b))))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Bishop on a piece of ICE protecting "
                                            (if hosted? (if remote? "a central" "a remote") "any") " server")
                                  :choices {:req #(if hosted?
                                                    (and (if remote?
                                                           (is-central? (second (:zone %)))
                                                           (is-remote? (second (:zone %))))
                                                         (ice? %)
                                                         (can-host? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                   (:hosted %)))
                                                    (and (ice? %)
                                                         (can-host? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not-any? (fn [c] (has-subtype? c "Caïssa"))
                                                                   (:hosted %))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}]
    :events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid (:host card))) (:rezzed target)))
              :effect (effect (ice-strength-bonus -2 target))}}}

   "Bug"
   {:implementation "Can only pay to see last card drawn after multiple draws"
    :req (req (some #{:hq} (:successful-run runner-reg)))
    :events {:corp-draw {:optional
                         {:prompt (msg "Pay 2 [Credits] to reveal card just drawn?")
                          :player :runner
                          :yes-ability {:msg (msg "reveal the card just drawn: " (:title (last (:hand corp))))
                                        :cost [:credit 2]}}}}}

   "Cache"
   {:abilities [{:counter-cost [:virus 1]
                 :effect (effect (gain-credits 1))
                 :msg "gain 1 [Credits]"}]
    :data {:counter {:virus 3}}}

   "Chakana"
   {:leave-play (effect (update-all-advancement-costs))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :virus 1))}
             :pre-advancement-cost {:req (req (>= (get-virus-counters state card) 3))
                                    :effect (effect (advancement-cost-bonus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (= (:cid target) (:cid card))))
              :effect (effect (update-all-advancement-costs))}
             :purge {:effect (effect (update-all-advancement-costs))}}}

   "Cloak"
   {:recurring 1}

   "Clot"
   {:effect (req (let [agendas (map first (filter #(is-type? (first %) "Agenda")
                                                  (turn-events state :corp :corp-install)))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))
    :events {:purge {:effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                                  (trash state side card {:cause :purge}))}
             :corp-install {:req (req (is-type? target "Agenda"))
                            :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons target %)))}}
    :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))}

   "Collective Consciousness"
   {:events {:rez {:req (req (ice? target)) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}

   "Copycat"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed current-ice)))
                 :effect (req (let [icename (:title current-ice)]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Choose a rezzed copy of " icename)
                                   :choices {:req #(and (rezzed? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
                                   :msg "redirect the run"
                                   :effect (req (let [dest (second (:zone target))
                                                      tgtndx (ice-index state target)]
                                                  (swap! state update-in [:run]
                                                         #(assoc % :position tgtndx :server [dest]))
                                                  (trash state side card {:cause :ability-cost})))}
                                 card nil)))}]}

   "Crescentus"
   {:implementation "Does not check that all subroutines were broken"
    :abilities [{:req (req (rezzed? current-ice))
                 :msg (msg "derez " (:title current-ice))
                 :effect (effect (trash card {:cause :ability-cost}) (derez current-ice))}]}

   "Customized Secretary"
   (letfn [(custsec-host [cards]
             {:prompt "Choose a program to host on Customized Secretary"
              :choices (cons "None" cards)
              :async true
              :effect (req (if (or (= target "None") (not (is-type? target "Program")))
                             (do (clear-wait-prompt state :corp)
                                 (shuffle! state side :deck)
                                 (system-msg state side (str "shuffles their Stack"))
                                 (effect-completed state side eid))
                             (do (host state side (get-card state card) target)
                                 (system-msg state side (str "hosts " (:title target) " on Customized Secretary"))
                                 (continue-ability state side (custsec-host (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:async true
      :interactive (req (some #(card-flag? % :runner-install-draw true) (all-active state :runner)))
      :msg (msg "reveal the top 5 cards of their Stack: " (join ", " (map :title (take 5 (:deck runner)))))
      :effect (req (show-wait-prompt state :corp "Runner to host programs on Customized Secretary")
                   (let [from (take 5 (:deck runner))]
                     (continue-ability state side (custsec-host from) card nil)))
      :abilities [{:cost [:click 1]
                   :prompt "Choose a program hosted on Customized Secretary to install"
                   :choices (req (cancellable (filter #(can-pay? state side nil :credit (:cost %))
                                                      (:hosted card))))
                   :msg (msg "install " (:title target))
                   :effect (req (when (can-pay? state side nil :credit (:cost target))
                                  (runner-install state side target)))}]})
   "Consume"
   {:events {:runner-trash {:async true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :effect (req (let [amt-trashed (count (filter #(card-is? % :side :corp) targets))
                                               auto-ab {:effect (effect (add-counter :runner card :virus amt-trashed))
                                                        :msg (str "place " (quantify amt-trashed "virus counter") " on Consume")}
                                               sing-ab {:optional {:prompt "Place a virus counter on Consume?"
                                                                   :yes-ability {:effect (effect (add-counter :runner card :virus 1))
                                                                                 :msg "place 1 virus counter on Consume"}}}
                                               mult-ab {:prompt "Place virus counters on Consume?"
                                                        :choices {:number (req amt-trashed)
                                                                  :default (req amt-trashed)}
                                                        :msg (msg "place " (quantify target "virus counter") " on Consume")
                                                        :effect (effect (add-counter :runner card :virus target))}
                                               ab (if (> amt-trashed 1) mult-ab sing-ab)
                                               ab (if (get-in card [:special :auto-accept]) auto-ab ab)]
                                           (continue-ability state side ab card targets)))}}
    :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Consume."))
    :abilities [{:req (req (pos? (get-virus-counters state card)))
                 :cost [:click 1]
                 :label "Gain 2 [Credits] for each hosted virus counter, then remove all virus counters."
                 :effect (req (gain-credits state side (* 2 (get-virus-counters state card)))
                              (update! state side (assoc-in card [:counter :virus] 0))
                              (when-let [hiveminds (filter #(= "Hivemind" (:title %)) (all-active-installed state :runner))]
                                        (doseq [h hiveminds]
                                               (update! state side (assoc-in h [:counter :virus] 0)))))
                 :msg (msg (let [local-virus (get-counters card :virus)
                                 global-virus (get-virus-counters state card)
                                 hivemind-virus (- global-virus local-virus)]
                             (str "gain " (* 2 global-virus) " [Credits], removing " (quantify local-virus "virus counter") " from Consume"
                             (when (pos? hivemind-virus)
                                   (str " (and " hivemind-virus " from Hivemind)")))))}
                {:effect (effect (update! (update-in card [:special :auto-accept] #(not %)))
                                 (toast (str "Consume will now "
                                             (if (get-in card [:special :auto-accept]) "no longer " "")
                                             "automatically add counters.") "info"))
                 :label "Toggle automatically adding virus counters"}]}

   "D4v1d"
   {:implementation "Does not check that ICE strength is 5 or greater"
    :data {:counter {:power 3}}
    :abilities [{:counter-cost [:power 1]
                 :msg "break 1 subroutine"}]}

   "DaVinci"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:effect
                 (req (let [c card]
                        (resolve-ability state side
                                         {:prompt "Choose a card to install from your Grip"
                                          :choices {:req #(and (<= (:cost %) (get-counters c :power))
                                                               (#{"Hardware" "Program" "Resource"} (:type %))
                                                               (in-hand? %))}
                                          :req (req (not (install-locked? state side)))
                                          :msg (msg "install " (:title target) " at no cost")
                                          :effect (effect (trash card {:cause :ability-cost})
                                                          (runner-install target {:ignore-install-cost true}))}
                                         card nil)))}]}

   "Datasucker"
   {:events (let [ds {:effect (req (update! state side (dissoc card :datasucker-count)))}]
              {:successful-run {:silent (req true)
                                :effect (effect (add-counter card :virus 1))
                                :req (req (#{:hq :rd :archives} target))}
               :pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                 (:datasucker-count card)))
                                  :effect (req (let [c (:datasucker-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice ds :run-ends ds})
    :abilities [{:counter-cost [:virus 1]
                 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req (and current-ice (:rezzed current-ice)))
                 :effect (req (update! state side (update-in card [:datasucker-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}]}

   "Deep Thought"
   {:events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :virus 1))}
             :runner-turn-begins
             {:interactive (req true)
              :req (req (>= (get-virus-counters state card) 3))
              :msg "look at the top card of R&D"
              :effect (effect (prompt! card (str "The top card of R&D is "
                                                 (:title (first (:deck corp))) ".")
                                       ["OK"] {}))}}}

   "Dhegdheer"
   {:abilities [{:label "Install a program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Dhegdheer"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg (str "host " (:title target)
                                                   (when (-> target :cost pos?)
                                                     ", lowering its cost by 1 [Credit]")))
                                    :effect (effect (when (-> target :cost pos?)
                                                      (install-cost-bonus state side [:credit -1]))
                                                    (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                                   card nil))}
                {:label "Host an installed program on Dhegdheer with [Credit] discount"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer with [Credit] discount"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg (str "host " (:title target)
                                (when (-> target :cost pos?)
                                  ", lowering its cost by 1 [Credit]")))
                 :effect (req (free-mu state (:memoryunits target))
                              (when (-> target :cost pos?)
                                (gain-credits state side 1))
                              (update-breaker-strength state side target)
                              (host state side card (get-card state target))
                              (update! state side (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}
                {:label "Host an installed program on Dhegdheer"
                 :req (req (nil? (get-in card [:special :dheg-prog])))
                 :prompt "Choose an installed program to host on Dhegdheer"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg (str "host " (:title target)
                                (when (-> target :cost pos?)
                                  ", lowering its cost by 1 [Credit]")))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card) [:special :dheg-prog] (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dheg-prog])))
                          :effect (effect (update! (dissoc-in card [:special :dheg-prog]))
                                          (use-mu (:memoryunits target)))}}}

   "Disrupter"
   {:events
    {:pre-init-trace
     {:async true
      :effect (effect (show-wait-prompt :corp "Runner to use Disrupter")
                      (continue-ability :runner
                        {:optional
                         {:prompt "Use Disrupter's ability?"
                          :yes-ability
                          {:effect (req (trash state side card {:cause :ability-cost})
                                        (swap! state assoc-in [:trace :force-base] 0))}
                          :end-effect (effect (clear-wait-prompt :corp))}}
                        card nil))}}}

   "Diwan"
   {:prompt "Choose the server that this copy of Diwan is targeting:"
    :choices (req servers)
    :effect (effect (update! (assoc card :server-target target)))
    :events {:purge {:effect (effect (trash card {:cause :purge}))}
             :pre-corp-install {:req (req (let [c target
                                                serv (:server (second targets))]
                                            (and (= serv (:server-target card))
                                                 (not (and (is-central? serv)
                                                           (is-type? c "Upgrade"))))))
                                :effect (effect (install-cost-bonus [:credit 1]))}}}

   "Djinn"
   {:abilities [{:label "Search your Stack for a virus program and add it to your Grip"
                 :prompt "Choose a Virus"
                 :msg (msg "add " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(and (is-type? % "Program")
                                                          (has-subtype? % "Virus"))
                                                    (:deck runner)) :sorted))
                 :cost [:click 1 :credit 1]
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (move target :hand))}
                {:label "Install a non-Icebreaker program on Djinn"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a non-Icebreaker program in your Grip to install on Djinn"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (not (has-subtype? % "Icebreaker"))
                                                         (in-hand? %))}
                                    :msg (msg "install and host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                  card nil))}
                {:label "Host an installed non-Icebreaker program on Djinn"
                 :prompt "Choose an installed non-Icebreaker program to host on Djinn"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}

   "Egret"
   {:implementation "Added subtypes don't get removed when Egret is moved/trashed"
    :hosting {:req #(and (ice? %) (can-host? %) (rezzed? %))}
    :msg (msg "make " (card-str state (:host card)) " gain Barrier, Code Gate and Sentry subtypes")
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:ice-strength-changed
             {:effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (assoc (:host (get-card state card)) :subtype (combine-subtypes false(-> card :host :subtype) "Barrier" "Code Gate" "Sentry")))
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (continue state side nil))}}}

   "Equivocation"
   (let [force-draw (fn [title]
                      {:optional {:prompt (str "Force the Corp to draw " title "?")
                                  :yes-ability {:async true
                                                :effect (req (show-wait-prompt state :runner "Corp to draw")
                                                             (wait-for (draw state :corp 1 nil)
                                                                       (do (system-msg state :corp (str "is forced to draw " title))
                                                                           (clear-wait-prompt state :runner)
                                                                           (effect-completed state side eid))))}}})
         reveal {:optional {:prompt "Reveal the top card of R&D?"
                            :yes-ability {:async true
                                          :effect (req (let [topcard (-> corp :deck first :title)]
                                                         (system-msg state :runner (str "reveals " topcard
                                                                                        " from the top of R&D"))
                                                         (continue-ability state side (force-draw topcard) card nil)))}}}]
     {:events {:successful-run {:req (req (= target :rd))
                                :async true
                                :interactive (req true)
                                :effect (effect (continue-ability reveal card nil))}}})

   "eXer"
   {:in-play [:rd-access 1]
    :events {:purge {:effect (effect (trash card {:cause :purge}))}}}

   "Expert Schedule Analyzer"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on HQ"
                 :makes-run true
                 :effect (effect (run :hq {:req (req (= target :hq))
                                           :replace-access
                                           {:msg (msg "reveal cards in HQ: "
                                                      (join ", " (map :title (:hand corp))))}} card))}]}

   "False Echo"
   {:abilities [{:req (req (and run
                                (< (:position run) (count run-ices))
                                (not (rezzed? current-ice))))
                 :msg "make the Corp rez the passed ICE or add it to HQ"
                 :effect (req (let [s (:server run)
                                    ice (nth (get-in @state (vec (concat [:corp :servers] s [:ices]))) (:position run))
                                    icename (:title ice)
                                    icecost (rez-cost state side ice)]
                                (continue-ability
                                  state side
                                  {:prompt (msg "Rez " icename " or add it to HQ?") :player :corp
                                   :choices (req (if (< (:credit corp) icecost)
                                                     ["Add to HQ"]
                                                     ["Rez" "Add to HQ"]))
                                   :effect (req (if (= target "Rez")
                                                  (rez state side ice)
                                                  (do (move state :corp ice :hand nil)
                                                      (system-msg state :corp (str "chooses to add the passed ICE to HQ"))))
                                                (trash state side card))}
                                 card nil)))}]}

   "Gorman Drip v1"
   {:abilities [{:cost [:click 1] :effect (effect (gain-credits (get-virus-counters state card))
                                                  (trash card {:cause :ability-cost}))
                 :msg (msg "gain " (get-virus-counters state card) " [Credits]")}]
    :events {:corp-click-credit {:effect (effect (add-counter :runner card :virus 1))}
             :corp-click-draw {:effect (effect (add-counter :runner card :virus 1))}}}

   "Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine" :effect (effect (trash card {:cause :ability-cost}))}]}

   "Gravedigger"
   {:events (let [e {:req (req (and (installed? target) (= (:side target) "Corp")))
                     :effect (effect (add-counter :runner card :virus 1))}]
              {:runner-trash e :corp-trash e})
    :abilities [{:counter-cost [:virus 1]
                 :cost [:click 1]
                 :msg "force the Corp to trash the top card of R&D"
                 :effect (effect (mill :corp))}]}

   "Harbinger"
   {:trash-effect
     {:req (req (not-any? #{:facedown :hand} (:previous-zone card)))
      :effect (req (let [lock (get-in @state [:runner :locked :discard])]
                     (swap! state assoc-in [:runner :locked] nil)
                     (runner-install state :runner card {:facedown true})
                     (swap! state assoc-in [:runner :locked] lock)))}}

   "Hemorrhage"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :virus 1))}}
    :abilities [{:counter-cost [:virus 2]
                 :cost [:click 1]
                 :req (req (pos? (count (:hand corp))))
                 :msg "force the Corp to trash 1 card from HQ"
                 :effect (req (show-wait-prompt state :runner "Corp to trash a card from HQ")
                              (resolve-ability
                                state :corp
                                {:prompt "Choose a card to trash"
                                 :choices (req (filter #(= (:side %) "Corp") (:hand corp)))
                                 :effect (effect (trash target)
                                                 (clear-wait-prompt :runner))}
                               card nil))}]}

   "Hivemind"
   (let [update-programs (req (let [virus-programs (->> (all-installed state :runner)
                                                  (filter #(and (program? %)
                                                                (has-subtype? % "Virus")
                                                                (not (facedown? %)))))]
                                (doseq [p virus-programs]
                                  (update-breaker-strength state side p))))]
     {:data {:counter {:virus 1}}
      :effect update-programs
      :trash-effect {:effect update-programs}
      :events {:counter-added {:req (req (= (:cid target) (:cid card)))
                               :effect update-programs}}
      :abilities [{:req (req (pos? (get-counters card :virus)))
                   :priority true
                   :prompt "Move a virus counter to which card?"
                   :choices {:req #(has-subtype? % "Virus")}
                   :effect (req (let [abilities (:abilities (card-def target))
                                      virus target]
                                  (add-counter state :runner virus :virus 1)
                                  (add-counter state :runner card :virus -1)
                                  (if (= (count abilities) 1)
                                    (do (swap! state update-in [side :prompt] rest) ; remove the Hivemind prompt so Imp works
                                      (resolve-ability state side (first abilities) (get-card state virus) nil))
                                    (resolve-ability
                                      state side
                                      {:prompt "Choose an ability to trigger"
                                       :choices (vec (map :msg abilities))
                                       :effect (req (swap! state update-in [side :prompt] rest)
                                                    (resolve-ability
                                                      state side
                                                      (first (filter #(= (:msg %) target) abilities))
                                                      card nil))}
                                      (get-card state virus) nil))))
                   :msg (msg "trigger an ability on " (:title target))}]})

   "Hyperdriver"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (move card :rfg) (gain :click 3))
                 :msg "gain [Click][Click][Click]"}]}

   "Imp"
   {:flags {:slow-trash (req (pos? (get-counters card :virus)))}
    :data {:counter {:virus 2}}
    :interactions {:trash-ability {:interactive (req true)
                                   :label "[Imp]: Trash card"
                                   :req (req (and (not (get-in @state [:per-turn (:cid card)]))
                                                  (pos? (get-counters card :virus))))
                                   :counter-cost [:virus 1]
                                   :msg (msg "trash " (:title target) " at no cost")
                                   :once :per-turn
                                   :async true
                                   :effect (effect (trash-no-cost eid target))}}}

   "Incubator"
   {:events {:runner-turn-begins {:silent (req true)
                                  :effect (effect (add-counter card :virus 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move " (get-counters card :virus) " virus counter to " (:title target))
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Virus"))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (add-counter target :virus (get-counters card :virus)))}]}

   "Ixodidae"
   {:events {:corp-credit-loss {:msg "gain 1 [Credits]"
                                :effect (effect (gain-credits :runner 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}

   "Keyhole"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on R&D"
                 :makes-run true
                 :effect (effect (run :rd
                                   {:req (req (= target :rd))
                                    :replace-access
                                    {:prompt "Choose a card to trash"
                                     :not-distinct true
                                     :msg (msg "trash " (:title target))
                                     :choices (req (take 3 (:deck corp)))
                                     :mandatory true
                                     :effect (effect (trash (assoc target :seen true))
                                                     (shuffle! :corp :deck))}} card))}]}

   "Kyuban"
   {:hosting {:req #(and (ice? %) (can-host? %))}
    :events {:pass-ice {:req (req (same-card? target (:host card)))
                        :msg "gain 2 [Credits]"
                        :effect (effect (gain-credits :runner 2))}}}

   "Lamprey"
   {:events {:successful-run {:req (req (= target :hq))
                              :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose-credits :corp 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}

   "Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program in your Grip to install on Leprechaun"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc-in (get-card state card)
                                                                    [:special :hosted-programs]
                                                                    (cons (:cid target)
                                                                          (get-in card [:special :hosted-programs])))))}
                                  card nil))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (< (count (get-in card [:special :hosted-programs])) 2))
                 :prompt "Choose an installed program to host on Leprechaun"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (free-mu (:memoryunits target))
                                 (update-breaker-strength target)
                                 (host card (get-card state target))
                                 (update! (assoc-in (get-card state card)
                                                    [:special :hosted-programs]
                                                    (cons (:cid target)
                                                          (get-in card [:special :hosted-programs])))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (get-in card [:special :hosted-programs])))
                          :effect (effect (update! (assoc-in card
                                                             [:special :hosted-programs]
                                                             (remove #(= (:cid target) %)
                                                                     (get-in card [:special :hosted-programs]))))
                                          (use-mu (:memoryunits target)))}}}

   "LLDS Energy Regulator"
   {:interactions {:prevent [{:type #{:trash-hardware}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1))}
                {:label "[Trash]: Prevent a hardware from being trashed"
                 :msg "prevent a hardware from being trashed"
                 :effect (effect (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}

   "Magnum Opus"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain-credits 2))
                 :msg "gain 2 [Credits]"}]}

   "Medium"
   {:events
    {:successful-run {:req (req (= target :rd))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:async true
                  :req (req (= target :rd))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state card)))
                                     :prompt "Choose how many additional R&D accesses to make with Medium"
                                     :choices {:number (req (dec (get-virus-counters state card)))
                                               :default (req (dec (get-virus-counters state card)))}
                                     :msg (msg "access " target " additional cards from R&D")
                                     :effect (effect (access-bonus :rd (max 0 target)))}
                                    card nil))}}}
   "Misdirection"
   {:abilities [{:cost [:click 2]
                 :prompt "How many [Credits] to spend to remove that number of tags?"
                 :choices {:number (req (min (:credit runner) (get-in runner [:tag :base])))}
                 :msg (msg "spend " target " [Credits] and remove " target " tags")
                 :effect (effect (lose-credits target)
                                 (lose-tags target))}]}

   "Multithreader"
   {:recurring 2}

   "Nerve Agent"
   {:events
    {:successful-run {:req (req (= target :hq))
                      :effect (effect (add-counter card :virus 1))}
     :pre-access {:async true
                  :req (req (= target :hq))
                  :effect (effect (continue-ability
                                    {:req (req (< 1 (get-virus-counters state card)))
                                     :prompt "Choose how many additional HQ accesses to make with Nerve Agent"
                                     :choices {:number (req (dec (get-virus-counters state card)))
                                               :default (req (dec (get-virus-counters state card)))}
                                     :msg (msg "access " target " additional cards from HQ")
                                     :effect (effect (access-bonus :hq (max 0 target)))}
                                    card nil))}}}

   "Net Shield"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}

   "Nyashia"
   {:data {:counter {:power 3}}
    :events {:pre-access {:async true
                          :req (req (and (pos? (get-counters card :power))
                                         (= target :rd)))
                          :effect (effect (show-wait-prompt :corp "Runner to use Nyashia")
                                          (continue-ability
                                            {:optional
                                             {:prompt "Spend a power counter on Nyashia to access 1 additional card?"
                                              :yes-ability {:msg "access 1 additional card from R&D"
                                                            :effect (effect (access-bonus :rd 1)
                                                                            (add-counter card :power -1)
                                                                            (clear-wait-prompt :corp))}
                                              :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                            card nil))}}}

   "Origami"
   {:effect (effect (gain :hand-size
                          {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                         (all-active-installed state :runner)))))}))
    :leave-play (effect (lose :hand-size
                              {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                             (all-active-installed state :runner)))))}))}

   "Paintbrush"
   {:abilities [{:cost [:click 1]
                 :choices {:req #(and (installed? %) (ice? %) (rezzed? %))}
                 :effect (req (let [ice target
                                    stypes (:subtype ice)]
                           (resolve-ability
                              state :runner
                              {:prompt (msg "Choose a subtype")
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "spend [Click] and make " (card-str state ice) " gain " (.toLowerCase target)
                                         " until the end of the next run this turn")
                               :effect (effect (update! (assoc ice :subtype (combine-subtypes true stypes target)))
                                               (update-ice-strength (get-card state ice))
                                               (register-events {:run-ends
                                                                 {:effect (effect (update! (assoc ice :subtype stypes))
                                                                                  (unregister-events card)
                                                                                  (update-ice-strength (get-card state ice)))}} card))}
                            card nil)))}]
    :events {:run-ends nil}}

   "Panchatantra"
   {:abilities [{:msg "add a custom subtype to currently encountered ICE"
                 :once :per-turn}]}

   "Parasite"
   {:hosting {:req #(and (ice? %) (can-host? %) (rezzed? %))}
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:runner-turn-begins
             {:silent (req true)
              :effect (req (add-counter state side card :virus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (= (:cid target) (:cid card))))
              :effect (effect (update-ice-strength (:host card)))}
             :pre-ice-strength
             {:req (req (= (:cid target) (:cid (:host card))))
              :effect (effect (ice-strength-bonus (- (get-virus-counters state card)) target))}
             :ice-strength-changed
             {:req (req (and (= (:cid target) (:cid (:host card)))
                             (not (card-flag? (:host card) :untrashable-while-rezzed true))
                             (<= (:current-strength target) 0)))
              :effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (trash state side target)
                           (continue state side nil))
              :msg (msg "trash " (:title target))}}}

   "Paricia"
   {:recurring 2}

   "Pawn"
   {:implementation "All abilities are manual"
    :abilities [{:label "Host Pawn on the outermost ICE of a central server"
                 :cost [:click 1]
                 :prompt "Host Pawn on the outermost ICE of a central server"
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Advance to next ICE"
                 :prompt "Choose the next innermost ICE to host Pawn on it"
                 :choices {:req #(and (ice? %)
                                      (can-host? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Trash Pawn and install a Caïssa from your Grip or Heap, ignoring all costs"
                 :effect (req (let [this-pawn (:cid card)]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a Caïssa program to install from your Grip or Heap"
                                   :show-discard true
                                   :choices {:req #(and (has-subtype? % "Caïssa")
                                                        (not= (:cid %) this-pawn)
                                                        (#{[:hand] [:discard]} (:zone %)))}
                                   :msg (msg "install " (:title target))
                                   :effect (effect (runner-install target {:ignore-all-cost true}))}
                                  card nil)
                                (trash state side card)))}]}

   "Plague"
   {:prompt "Choose a server for Plague"
    :choices (req servers)
    :msg (msg "target " target)
    :req (req (not (get-in card [:special :server-target])))
    :effect (effect (update! (assoc-in card [:special :server-target] target)))
    :events {:successful-run
             {:req (req (= (zone->name (get-in @state [:run :server]))
                           (get-in (get-card state card) [:special :server-target])))
              :msg "gain 2 virus counters"
              :effect (effect (add-counter :runner card :virus 2))}}}

   "Pheromones"
   {:recurring (req (when (< (get-counters card :recurring) (get-counters card :virus))
                      (set-prop state side card :rec-counter (get-counters card :virus))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :hq))
                              :effect (effect (add-counter card :virus 1))}}}

   "Progenitor"
   {:abilities [{:label "Install a virus program on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a Virus program to install on Progenitor"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (has-subtype? % "Virus")
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target))
                                    :effect (effect (runner-install target {:host-card card :no-mu true})
                                                    (update! (assoc (get-card state card)
                                                                    :hosted-programs
                                                                    (cons (:cid target) (:hosted-programs card)))))}
                                  card nil))}
                {:label "Host an installed virus on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed virus program to host on Progenitor"
                 :choices {:req #(and (is-type? % "Program")
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:pre-purge {:effect (req (when-let [c (first (:hosted card))]
                                        (update! state side (assoc-in card [:special :numpurged] (get-counters c :virus)))))}
             :purge {:req (req (pos? (get-in card [:special :numpurged] 0)))
                     :effect (req (when-let [c (first (:hosted card))]
                                    (add-counter state side c :virus 1)))}
             :card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}

   "Reaver"
   {:events {:runner-trash {:req (req (and (first-installed-trash? state side)
                                           (installed? target)))
                            :effect (effect (draw :runner 1))
                            :msg "draw 1 card"}}}

   "RNG Key"
   {:events {:pre-access-card {:req (req (get-in card [:special :rng-guess]))
                               :async true
                               :msg (msg "to reveal " (:title target))
                               :effect (req (if-let [guess (get-in card [:special :rng-guess])]
                                              (if (installed? target)
                                                ;; Do not trigger on installed cards (can't "reveal" an installed card per UFAQ)
                                                (do (toast state :runner "Installed cards cannot be revealed, so RNG Key does not pay out." "info")
                                                    (effect-completed state side eid))
                                                (if (or (= guess (:cost target))
                                                        (= guess (:advancementcost target)))
                                                  (continue-ability state side
                                                                    {:prompt "Choose RNG Key award"
                                                                     :choices ["Gain 3 [Credits]" "Draw 2 cards"]
                                                                     :effect (req (if (= target "Draw 2 cards")
                                                                                    (do (draw state :runner 2)
                                                                                        (system-msg state :runner "uses RNG Key to draw 2 cards"))
                                                                                    (do (gain-credits state :runner 3)
                                                                                        (system-msg state :runner "uses RNG Key to gain 3 [Credits]"))))}
                                                                    card nil)
                                                  (effect-completed state side eid)))
                                              (effect-completed state side eid)))}
             :post-access-card {:effect (effect (update! (assoc-in card [:special :rng-guess] nil)))}
             :successful-run {:req (req (and (#{:hq :rd} target)
                                             (first-event? state :runner :successful-run #{[:hq] [:rd]})))
                              :optional {:prompt "Fire RNG Key?"
                                         :yes-ability {:prompt "Guess a number"
                                                       :choices {:number (req 20)}
                                                       :msg (msg "guess " target)
                                                       :effect (effect (update! (assoc-in card [:special :rng-guess] target)))}}}}}

   "Rook"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [r (get-card state card)
                                    hosted? (ice? (:host r))
                                    icepos (ice-index state (get-card state (:host r)))]
                                (resolve-ability state side
                                 {:prompt (if hosted?
                                            (msg "Host Rook on a piece of ICE protecting this server or at position "
                                              icepos " of a different server")
                                            (msg "Host Rook on a piece of ICE protecting any server"))
                                  :choices {:req #(if hosted?
                                                    (and (or (= (:zone %) (:zone (:host r)))
                                                             (= (ice-index state %) icepos))
                                                         (= (last (:zone %)) :ices)
                                                         (ice? %)
                                                         (can-host? %)
                                                         (not-any? (fn [c] (has? c :subtype "Caïssa")) (:hosted %)))
                                                    (and (ice? %)
                                                         (can-host? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not-any? (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}]
    :events {:pre-rez-cost {:req (req (= (:zone (:host card)) (:zone target)))
                            :effect (effect (rez-cost-bonus 2))}}}

   "Sahasrara"
   {:recurring 2}

   "Savoir-faire"
   {:abilities [{:cost [:credit 2]
                 :once :per-turn
                 :req (req (not (install-locked? state side)))
                 :msg (msg "install " (:title target))
                 :prompt "Choose a program to install from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :effect (effect (runner-install target))}]}

   "Scheherazade"
   {:abilities [{:label "Install and host a program from Grip"
                 :effect (effect (resolve-ability
                                   {:cost [:click 1]
                                    :prompt "Choose a program to install on Scheherazade from your grip"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "host " (:title target) " and gain 1 [Credits]")
                                    :effect (effect (runner-install target {:host-card card}) (gain-credits 1))}
                                  card nil))}
                {:label "Host an installed program"
                 :prompt "Choose a program to host on Scheherazade" :priority 2
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (req (when (host state side card target)
                                (gain-credits state side 1)))}]}

   "Self-modifying Code"
   {:abilities [{:req (req (not (install-locked? state side)))
                 :effect (req (wait-for (trash state side card {:cause :ability-cost})
                                        (continue-ability state side
                                                          {:prompt "Choose a program to install"
                                                           :msg (req (if (not= target "No install")
                                                                       (str "install " (:title target))
                                                                       (str "shuffle their Stack")))
                                                           :priority true
                                                           :choices (req (cancellable
                                                                           (conj (vec (sort-by :title (filter #(is-type? % "Program")
                                                                                                              (:deck runner))))
                                                                                 "No install")))
                                                           :cost [:credit 2]
                                                           :effect (req (trigger-event state side :searched-stack nil)
                                                                        (shuffle! state side :deck)
                                                                        (when (not= target "No install")
                                                                          (runner-install state side target)))} card nil)))}]}

   "Sneakdoor Beta"
   {:abilities [{:cost [:click 1]
                 :msg "make a run on Archives"
                 :makes-run true
                 :effect (effect (run :archives
                                   {:req (req (= target :archives))
                                    :successful-run
                                    {:silent (req true)
                                     :effect (req (swap! state assoc-in [:run :server] [:hq])
                                                  ; remove the :req from the run-effect, so that other cards that replace
                                                  ; access don't use Sneakdoor's req. (Security Testing, Ash 2X).
                                                  (swap! state dissoc-in [:run :run-effect :req])
                                                  (trigger-event state :corp :no-action)
                                                  (system-msg state side
                                                              (str "uses Sneakdoor Beta to make a successful run on HQ")))}}
                                   card))}]}

   "Snitch"
   {:abilities [{:once :per-run :req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :async true
                 :effect (req (wait-for (expose state side current-ice)
                                        (continue-ability
                                          state side
                                          {:optional {:prompt "Jack out?"
                                                      :yes-ability {:msg "jack out"
                                                                    :effect (effect (jack-out nil))}
                                                      :no-ability {:msg "continue the run"}}}
                                          card nil)))}]}

   "Surfer"
   (letfn [(surf [state cice]
             {:prompt (msg "Choose an ICE before or after " (:title cice))
              :choices {:req #(and (ice? %)
                                   (= (:zone %) (:zone cice))
                                   (= 1 (abs (- (ice-index state %)
                                                (ice-index state cice)))))}
              :effect (req (let [tgtndx (ice-index state target)
                                 cidx (ice-index state cice)]
                             (system-msg state :runner (str "uses Surfer to swap "
                                                            (card-str state cice)
                                                            " and "
                                                            (card-str state (nth run-ices tgtndx))))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % tgtndx cice))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % cidx target))
                             (swap! state update-in [:run] #(assoc % :position (inc tgtndx)))
                             (update-all-ice state side)
                             (trigger-event state side :approach-ice current-ice)))})]
     {:abilities [{:cost [:credit 2]
                   :msg "swap a piece of Barrier ICE"
                   :req (req (and (:run @state)
                                  (rezzed? current-ice)
                                  (has-subtype? current-ice "Barrier")))
                   :label "Swap the Barrier ICE currently being encountered with a piece of ICE directly before or after it"
                   :effect (effect (resolve-ability (surf state current-ice) card nil))}]})

   "Takobi"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg "adds a power counter to Takobi"))}
                {:req (req (and (:run @state)
                                (rezzed? current-ice)
                                (>= (get-counters card :power) 2)))
                 :counter-cost [:power 2]
                 :label "Increase non-AI icebreaker strength by +3 until end of encounter"
                 :prompt "Choose an installed non-AI icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "add +3 strength to " (:title target) " for remainder of encounter")
                 :effect (effect (pump target 3 :encounter))}]}

   "Tapwrm"
   (let [ability {:label "Gain [Credits] (start of turn)"
                  :msg (msg "gain " (quot (:credit corp) 5) " [Credits]")
                  :once :per-turn
                  :silent (req true)
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (gain-credits (quot (:credit corp) 5)))}]
     {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
      :flags {:drip-economy true}
      :abilities [ability]
      :events {:runner-turn-begins ability
               :purge {:effect (effect (trash card {:cause :purge}))}}})

   "Tracker"
   (let [ability {:prompt "Choose a server for Tracker"
                  :choices (req servers)
                  :interactive (req true)
                  :msg (msg "target " target)
                  :req (req (not (:server-target card)))
                  :effect (effect (update! (assoc card :server-target target)))}]
     {:implementation "Prevention is manual"
      :abilities [{:label "Make a run on targeted server"
                   :cost [:click 1 :credit 2]
                   :req (req (some #(= (:server-target card) %) runnable-servers))
                   :msg (msg "make a run on " (:server-target card) ". Prevent the first subroutine that would resolve from resolving")
                   :effect (effect (run (:server-target card) nil card))}]
      :events {:runner-turn-begins ability
               :runner-turn-ends {:effect (effect (update! (dissoc card :server-target)))}}})

   "Trope"
   {:events {:runner-turn-begins {:silent (req true)
                                  :effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :label "[Click], remove Trope from the game: Reshuffle cards from Heap back into Stack"
                 :effect (effect
                          (move card :rfg)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max (min (get-counters card :power) (count (:discard runner)))
                                      :all true
                                      :req #(and (= (:side %) "Runner")
                                                 (in-discard? %))}
                            :msg (msg "shuffle " (join ", " (map :title targets))
                                      " into their Stack")
                            :effect (req (doseq [c targets]
                                           (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}

   "Trypano"
   (let [ability {:label "Place a virus counter on Trypano (start of turn)"
                  :async true
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (system-msg "places a virus counter on Trypano")
                                  (add-counter card :virus 1)
                                  (effect-completed eid))}
         trash-if-5 (req (when-let [h (get-card state (:host card))]
                           (if (and (>= (get-virus-counters state card) 5)
                                    (not (and (card-flag? h :untrashable-while-rezzed true)
                                              (rezzed? h))))
                             (do (system-msg state :runner (str "uses Trypano to trash " (card-str state h)))
                                 (unregister-events state side card)
                                 (trash state :runner eid h nil))
                             (effect-completed state side eid))))
         trash-if-5-ability {:async true
                             :effect trash-if-5}]
       {:flags {:runner-phase-12 (req true)}
        :hosting {:req #(and (ice? %)
                             (can-host? %))}
        :effect trash-if-5
        :abilities [ability]
        :events {:runner-turn-begins
                 {:interactive (req true)
                  :optional {:prompt (msg "Place a virus counter on Trypano?")
                             :yes-ability ability}}
                 :counter-added trash-if-5-ability
                 :card-moved trash-if-5-ability
                 :runner-install trash-if-5-ability}})

   "Upya"
   {:implementation "Power counters added automatically"
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:power 3]
                 :once :per-turn
                 :msg "gain [Click][Click]"
                 :effect (effect (gain :click 2))}]}

   "Wari"
   (letfn [(prompt-for-subtype []
             {:prompt "Choose a subtype"
              :choices ["Barrier" "Code Gate" "Sentry"]
              :async true
              :effect (req (wait-for (trash state side card {:unpreventable true})
                                     (continue-ability state side
                                                       (expose-and-maybe-bounce target)
                                                       card nil)))})
           (expose-and-maybe-bounce [chosen-subtype]
             {:choices {:req #(and (ice? %) (not (rezzed? %)))}
              :async true
              :msg (str "name " chosen-subtype)
              :effect (req (wait-for (expose state side target)
                                     (do (if (and async-result
                                                  (has-subtype? target chosen-subtype))
                                           (do (move state :corp target :hand)
                                               (system-msg state :runner
                                                           (str "add " (:title target) " to HQ"))))
                                         (effect-completed state side eid))))})]
     {:events {:successful-run
              {:interactive (req true)
               :async true
               :req (req (and (= target :hq)
                              (first-successful-run-on-server? state :hq)
                              (some #(and (ice? %) (not (rezzed? %)))
                                    (all-installed state :corp))))
               :effect (effect (continue-ability
                                {:prompt "Use Wari?"
                                 :choices ["Yes" "No"]
                                 :async true
                                 :effect (req (if (= target "Yes")
                                                (continue-ability state side
                                                                  (prompt-for-subtype)
                                                                  card nil)
                                                (effect-completed state side eid)))}
                                card nil))}}})})
