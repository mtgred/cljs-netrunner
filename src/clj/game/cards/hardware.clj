(ns game.cards.hardware
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]
            [clojure.set :as clj-set]))

;; Card definitions

(defcard "Acacia"
  {:events [{:event :pre-purge
             :effect (req (let [counters (number-of-virus-counters state)]
                            (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
            {:event :purge
             :async true
             :effect (effect (show-wait-prompt  :corp "Runner to decide if they will use Acacia")
                             (continue-ability
                               {:optional
                                {:player :runner
                                 :prompt "Use Acacia?"
                                 :yes-ability
                                 {:async true
                                  :effect (req (let [counters (- (get-in (get-card state card) [:special :numpurged])
                                                                 (number-of-virus-counters state))]
                                                 (wait-for (trash state side card nil)
                                                           (gain-credits state side counters)
                                                           (system-msg state side (str "trashes Acacia and gains " counters "[Credit]"))
                                                           (clear-wait-prompt state :corp)
                                                           (effect-completed state side eid))))}
                                 :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                               card nil))}]})

(defcard "Adjusted Matrix"
  {:implementation "Click Adjusted Matrix to use ability."
   :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
   :prompt "Choose Icebreaker on which to install Adjusted Matrix"
   :choices {:card #(and (runner? %)
                         (has-subtype? % "Icebreaker")
                         (installed? %))}
   :msg (msg "host it on " (card-str state target))
   :effect (effect (update! (assoc target :subtype (combine-subtypes false (-> target :subtype) "AI")))
                   (host (get-card state target) (get-card state card)))
   :abilities [(break-sub [:click 1] 1 "All" {:req (req true)})]
   :events [{:event :pre-card-moved
             :req (req (same-card? target card))
             :effect (effect (update! (assoc (-> card :host) :subtype (-> card :host :subtype (remove-subtypes-once ["AI"])))))}]})

(defcard "Akamatsu Mem Chip"
  {:in-play [:memory 1]})

(defcard "Aniccam"
  (let [ability {:async true
                 :req (req (and (some event? targets)
                                (letfn [(event-targets? [event-targets]
                                          (some event? event-targets))]
                                  (= 1 (+ (event-count state side :runner-trash event-targets?)
                                          (event-count state side :corp-trash event-targets?))))))
                 :msg "draw 1 card"
                 :effect (effect (draw :runner eid 1 nil))}]
    {:in-play [:memory 1]
     :events [(assoc ability :event :corp-trash)
              (assoc ability :event :runner-trash)]}))

(defcard "Archives Interface"
  {:events
   [{:event :pre-access
     :async true
     :interactive (req true)
     :req (req (and (= target :archives)
                    (not= (:max-access run) 0)
                    (not-empty (:discard corp))))
     :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                  (continue-ability
                    state side
                    {:optional
                     {:prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                      :yes-ability {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                    :choices (req (:discard corp))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :corp target :rfg))}}} card nil))}]})

(defcard "Astrolabe"
  {:in-play [:memory 1]
   :events [{:event :server-created
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1 nil))}]})

(defcard "Autoscripter"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (and (program? target)
                            ;; only trigger on Runner's turn
                            (= (:active-player @state) :runner)
                            ;; only trigger when playing a Program from grip
                            (some #{:hand} (:previous-zone target))
                            ;; check that we haven't played a Program from the grip this turn
                            ;; which translates to just one case of playing a Program in turn-events
                            (first-event? state :runner :runner-install
                                          (fn [[card _]] (and (some #{:hand} (:previous-zone card))
                                                              (program? card))))))
             :msg "gain [Click]"
             :effect (effect (gain :click 1))}
            {:event :unsuccessful-run
             :async true
             :effect (effect (system-msg "trashes Autoscripter")
                             (trash eid card nil))}]})

(defcard "Blackguard"
  {:in-play [:memory 2]
   :events [{:event :expose
             :msg (msg "attempt to force the rez of " (:title target))
             :async true
             :effect (req (let [c target
                                cdef (card-def c)
                                cname (:title c)
                                cost (rez-cost state side target)
                                additional-costs (rez-additional-cost-bonus state side target)]
                            (if (seq additional-costs)
                              (do (show-wait-prompt state :runner (str "Corp to decide if they will rez " cname))
                                  (continue-ability
                                    state side
                                    {:optional
                                     {:prompt (msg (build-cost-string [:credit cost])
                                                   ", plus " (string/lower-case (build-cost-string additional-costs))
                                                   " as an additional cost to rez " cname "?")
                                      :player :corp
                                      :yes-ability {:effect (effect (rez :corp c))}
                                      :no-ability {:msg (msg "declines to pay additional costs"
                                                             " and is not forced to rez " cname)}
                                      :end-effect (effect (clear-wait-prompt :runner))}}
                                    card nil))
                              (do (rez state :corp target)
                                  (effect-completed state side eid)))))}]})

(defcard "Bookmark"
  {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                :cost [:click 1]
                :msg "host up to 3 cards from their Grip facedown"
                :choices {:max 3
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (doseq [c targets]
                               (host state side (get-card state card) c {:facedown true})))}
               {:label "Add all hosted cards to Grip"
                :cost [:click 1]
                :msg "add all hosted cards to their Grip"
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand)))}
               {:label "Add all hosted cards to Grip"
                :trash-icon true
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand))
                             (continue-ability
                               state side
                               {:cost [:trash]
                                :msg "add all hosted cards to their Grip"}
                               (get-card state card) nil))}]})

(defcard "Boomerang"
  {:prompt "Choose an installed piece of ice"
   :msg (msg "target " (card-str state target))
   :choices {:card #(and (installed? %)
                         (ice? %))}
   :effect (effect (add-icon card target "B" "blue")
                   (update! (assoc-in (get-card state card) [:special :boomerang-target] target)))
   :leave-play (effect (remove-icon card))
   :abilities [(assoc
                 (break-sub
                   [:trash] 2 "All"
                   {:req (req (if-let [boomerang-target (get-in card [:special :boomerang-target])]
                                (same-card? current-ice boomerang-target)
                                true))}) ; When eg. flipped by Assimilator
                 :effect
                 (req (wait-for
                        (trash state side (make-eid state eid) card {:cause :ability-cost
                                                                     :unpreventable true})
                        (continue-ability
                          state :runner
                          (when-let [[boomerang] async-result]
                            (break-sub
                              nil 2 "All"
                              {:additional-ability
                               {:effect
                                (effect
                                  (register-events
                                    boomerang
                                    [{:event :run-ends
                                      :once :per-run
                                      :duration :end-of-run
                                      :optional
                                      {:req (req (and (:successful target)))
                                       :prompt (msg "Shuffle a copy of " (:title card) " back into the Stack?")
                                       :yes-ability
                                       {:msg (msg "shuffle a copy of " (:title card) " back into the Stack")
                                        :effect (effect (move card :deck)
                                                        (shuffle! :deck))}}}]))}}))
                          card nil))))]})

(defcard "Box-E"
  {:in-play [:memory 2 :hand-size 2]})

(defcard "Brain Cage"
  {:in-play [:hand-size 3]
   :effect (effect (damage eid :brain 1 {:card card}))})

(defcard "Brain Chip"
  (let [runner-points (fn [s] (max (get-in s [:runner :agenda-point] 0) 0))]
    {:effect (req (gain state :runner
                        :memory (runner-points @state)
                        :hand-size (runner-points @state))
                  (add-watch state (keyword (str "brainchip" (:cid card)))
                             (fn [k ref old new]
                               (let [bonus (- (runner-points new) (runner-points old))]
                                 (when-not (zero? bonus)
                                   (gain state :runner
                                         :memory bonus
                                         :hand-size bonus))))))
     :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                      (lose state :runner
                            :memory (runner-points @state)
                            :hand-size (runner-points @state)))}))

(defcard "Buffer Drive"
  (let [triggered-ability
        {:once :per-turn
         :req (req (letfn [(grip-or-stack-trash? [event] ;; a <side>-trash event is a list of targets for trashing
                             (some #(and (runner? %)
                                         (or (in-hand? %) (in-deck? %)))
                                   event))]
                     (and (some #(and (runner? %)
                                      (or (in-hand? %)
                                          (in-deck? %)))
                                targets)
                       (= 1 (+ (event-count state side :runner-trash grip-or-stack-trash?)
                               (event-count state side :corp-trash grip-or-stack-trash?))))))
         :prompt "Add a trashed card to the bottom of the stack"
         :choices (req (conj (sort (map :title (filter :cid targets))) "No action"))
         :async true
         :effect (req (if (= "No action" target)
                        (effect-completed state side eid)
                        (do (system-msg state side
                                        (str "uses Buffer Drive to add " target
                                             " to the bottom of the stack"))
                            (move state side (find-card target (:discard (:runner @state))) :deck)
                            (effect-completed state side eid))))}]
    {:events [(assoc triggered-ability :event :runner-trash)
              (assoc triggered-ability :event :corp-trash)]
     :abilities [{:label "Add a card from the heap to the top of the stack"
                  :cost [:remove-from-game]
                  :show-discard true
                  :choices {:card #(and (runner? %)
                                        (in-discard? %))}
                  :msg (msg "add " (:title target) " to the top of the stack")
                  :effect (effect (move target :deck {:front true}))}]}))

(defcard "Capstone"
  {:abilities [{:req (req (pos? (count (:hand runner))))
                :label "trash and install cards"
                :cost [:click 1]
                :async true
                :prompt "Select any number of cards to trash from your grip"
                :choices {:max (req (count (:hand runner)))
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (let [trashed-card-names (keep :title targets)
                                   installed-card-names (keep :title (all-active-installed state :runner))
                                   overlap (clj-set/intersection (set trashed-card-names)
                                                                 (set installed-card-names))]
                               (wait-for (trash-cards state side targets {:unpreventable true})
                                         (let [trashed-cards async-result]
                                           (wait-for (draw state side (count (filter overlap trashed-card-names)) nil)
                                                     (system-msg state side
                                                                 (str "spends [Click] to use Capstone to trash "
                                                                      (string/join ", " (map :title trashed-cards))
                                                                      " from the grip and draw "
                                                                      (quantify (count async-result) "card")))
                                                     (effect-completed state side eid))))))}]})

(defcard "Chop Bot 3000"
  {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
   :abilities [{:req (req (:runner-phase-12 @state))
                :label "trash and draw or remove tag"
                :msg (msg "trash " (:title target))
                :choices {:card #(and (runner? %)
                                      (installed? %))
                          :not-self true}
                :async true
                :effect (req (wait-for (trash state :runner target nil)
                                       (continue-ability
                                         state side
                                         (let [deck (pos? (count (:deck runner)))
                                               tags (pos? (count-tags state))]
                                           {:req (req (or deck tags))
                                            :prompt "Draw 1 card or remove 1 tag"
                                            :choices (concat (when deck ["Draw 1 card"])
                                                             (when tags ["Remove 1 tag"]))
                                            :async true
                                            :effect (req (if (= target "Draw 1 card")
                                                           (draw state :runner eid 1 nil)
                                                           (lose-tags state :runner eid 1)))})
                                         card nil)))}]})

(defcard "Clone Chip"
  {:abilities [{:prompt "Select a program to install from your Heap"
                :label "install card from heap"
                :show-discard true
                :req (req (and (not (seq (get-in @state [:runner :locked :discard])))
                               (not (install-locked? state side))
                               (some #(and (program? %)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side %)]))
                                     (:discard runner))))
                :choices {:req (req (and (program? target)
                                         (in-discard? target)
                                         (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                   [:credit (install-cost state side target)])))}
                :cost [:trash]
                :msg (msg "install " (:title target))
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

(defcard "Comet"
  {:in-play [:memory 1]
   :events [{:event :play-event
             :req (req (first-event? state side :play-event))
             :effect (req (system-msg state :runner
                                      (str "can play another event without spending a [Click] by clicking on Comet"))
                          (update! state side (assoc card :comet-event true)))}]
   :abilities [{:async true
                :label "play an event in hand twice"
                :req (req (:comet-event card))
                :prompt "Select an Event in your Grip to play"
                :choices {:card #(and (event? %)
                                      (in-hand? %))}
                :msg (msg "play " (:title target))
                :effect (effect (update! (dissoc (get-card state card) :comet-event))
                                (play-instant eid target nil))}]})

(defcard "Cortez Chip"
  {:abilities [{:prompt "Select a piece of ICE"
                :label "increase rez cost of ice"
                :choices {:card #(and (ice? %)
                                      (not (rezzed? %)))}
                :msg (msg "increase the rez cost of " (card-str state target)
                          " by 2 [Credits] until the end of the turn")
                :cost [:trash]
                :effect (effect (register-floating-effect
                                  card
                                  (let [ice target]
                                    {:type :rez-additional-cost
                                     :duration :end-of-turn
                                     :req (req (same-card? target ice))
                                     :value [:credit 2]})))}]})

(defcard "Cyberdelia"
  {:in-play [:memory 1]
   :events [{:event :subroutines-broken
             :req (req (and (every? :broken (:subroutines target))
                            (first-event? state side :subroutines-broken #(every? :broken (:subroutines (first %))))))
             :msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
             :effect (effect (gain-credits 1))}]})

(defcard "Cyberfeeder"
  {:recurring 1
   :interactions {:pay-credits {:req (req (or (and (= :runner-install (:source-type eid))
                                                   (has-subtype? target "Virus")
                                                   (program? target))
                                              (and (= :ability (:source-type eid))
                                                   (has-subtype? target "Icebreaker"))))
                                :type :recurring}}})

(defcard "CyberSolutions Mem Chip"
  {:in-play [:memory 2]})

(defcard "Cybsoft MacroDrive"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Daredevil"
  {:in-play [:memory 2]
   :events [{:event :run
             :once :per-turn
             :req (req (and (<= 2 run-position)
                            (first-event? state side :run #(<= 2 (second %)))))
             :msg "draw two cards"
             :async true
             :effect (effect (draw eid 2 nil))}]})

(defcard "Dedicated Processor"
  {:implementation "Click Dedicated Processor to use ability"
   :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
   :hosting {:card #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
   :abilities [{:cost [:credit 2]
                :label "add 4 strength for the remainder of the run"
                :req (req run)
                :effect (effect (pump (get-card state (:host card)) 4))
                :msg (msg "pump the strength of " (get-in card [:host :title]) " by 4")}]})

(defcard "Deep Red"
  {:implementation "MU use restriction not enforced"
   :in-play [:memory 3]
   :events [{:event :runner-install
             :optional
             {:req (req (has-subtype? target "Caïssa"))
              :prompt "Use Deep Red?"
              :yes-ability {:async true
                            :effect (req (let [cid (:cid target)]
                                           (continue-ability
                                             state side
                                             {:async true
                                              :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                                              :choices {:card #(= cid (:cid %))}
                                              :msg (msg "trigger the [Click] ability of " (:title target)
                                                        " without spending [Click]")
                                              :effect (req (gain state :runner :click 1)
                                                           (play-ability state side {:card target :ability 0})
                                                           (effect-completed state side eid))}
                                             card nil)))}}}]})

(defcard "Demolisher"
  {:in-play [:memory 1]
   :constant-effects [{:type :trash-cost
                       :value -1}]
   :events [{:event :runner-trash
             :once :per-turn
             :req (req (some corp? targets))
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]})

(defcard "Desperado"
  {:in-play [:memory 1]
   :events [{:event :successful-run
             :silent (req true)
             :msg "gain 1 [Credits]" :effect (effect (gain-credits 1))}]})

(defcard "Devil Charm"
  {:events [{:event :encounter-ice
             :interactive (req true)
             :optional
             {:prompt "Remove this card from the game: give encountered ice -6 strength?"
              :yes-ability
              {:msg (msg "give -6 strength to " (card-str state target) " for the remainder of the run")
               :cost [:remove-from-game]
               :effect (effect (register-floating-effect
                                 card
                                 (let [target-ice target]
                                   {:type :ice-strength
                                    :duration :end-of-run
                                    :req (req (same-card? target target-ice))
                                    :value -6}))
                               (update-all-ice))}}}]})

(defcard "Dinosaurus"
  {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                :req (req (empty? (:hosted card))) :cost [:click 1]
                :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                :async true
                :effect (effect (update! (assoc-in (get-card state card) [:special :dino-breaker] (:cid target)))
                                (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed non-AI icebreaker on Dinosaurus"
                :req (req (empty? (:hosted card)))
                :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (req (free-mu state (:memoryunits target))
                             (->> target
                                  (get-card state)
                                  (host state side card)
                                  (update-breaker-strength state side))
                             (update! state side (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}]
   :constant-effects [{:type :breaker-strength
                       :req (req (same-card? target (first (:hosted card))))
                       :value 2}]
   :events [{:event :card-moved
             :req (req (= (:cid target) (get-in (get-card state card) [:special :dino-breaker])))
             :effect (effect (update! (dissoc-in card [:special :dino-breaker]))
                             (use-mu (:memoryunits target)))}]})

(defcard "Doppelgänger"
  {:in-play [:memory 1]
   :events [{:event :runner-install
             :req (req (= card target))
             :silent (req true)
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :runner-turn-begins
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :run-ends
             :interactive (req true)
             :optional
             {:req (req (and (:successful target)
                             (:dopp-active card)))
              :player :runner
              :prompt "Use Doppelgänger to run again?"
              :yes-ability {:prompt "Choose a server"
                            :async true
                            :choices (req runnable-servers)
                            :msg (msg "make a run on " target)
                            :makes-run true
                            :effect (effect (update! (dissoc card :dopp-active))
                                            (unregister-floating-effects :end-of-run)
                                            (unregister-floating-events :end-of-run)
                                            (update-all-icebreakers)
                                            (update-all-ice)
                                            (reset-all-ice)
                                            (clear-wait-prompt :corp)
                                            (make-run eid target))}}}]})

(defcard "Dorm Computer"
  {:data {:counter {:power 4}}
   :abilities [{:cost [:click 1 :power 1]
                :req (req (not run))
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg "make a run and avoid all tags for the remainder of the run"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-tag
                                    :duration :end-of-run
                                    :async true
                                    :msg "avoid all tags during the run"
                                    :effect (effect (tag-prevent :runner eid Integer/MAX_VALUE))}])
                                (make-run eid target nil card))}]})

(defcard "Dyson Fractal Generator"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Fracter")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Dyson Mem Chip"
  {:in-play [:memory 1]
   :constant-effects [{:type :link
                       :value 1}]})

(defcard "e3 Feedback Implants"
  {:abilities [(break-sub 1 1 "All" {:req (req true)})]})

(defcard "Ekomind"
  (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
    {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                  (add-watch state :ekomind (fn [k ref old new]
                                              (let [hand-size (count (get-in new [:runner :hand]))]
                                                (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                  (update-base-mu ref hand-size))))))
     :leave-play (req (remove-watch state :ekomind))}))

(defcard "EMP Device"
  {:abilities [{:req (req run)
                :msg "prevent the Corp from rezzing more than 1 piece of ICE for the remainder of the run"
                :cost [:trash]
                :effect (effect
                          (register-events
                            card
                            [{:event :rez
                              :duration :end-of-run
                              :unregister-once-resolved true
                              :req (req (ice? target))
                              :effect (effect (register-run-flag!
                                                card :can-rez
                                                (fn [state side card]
                                                  (if (ice? card)
                                                    ((constantly false)
                                                     (toast state :corp "Cannot rez ICE the rest of this run due to EMP Device"))
                                                    true))))}]))}]})

(defcard "Feedback Filter"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:credit 3]
                :msg "prevent 1 net damage"
                :effect (effect (damage-prevent :net 1))}
               {:label "Prevent up to 2 brain damage"
                :msg "prevent up to 2 brain damage"
                :cost [:trash]
                :effect (effect (damage-prevent :brain 2))}]})

(defcard "Flame-out"
  (let [turn-end {:async true
                  :effect (req (unregister-events state :runner card)
                               (if-let [hosted (first (:hosted card))]
                                 (do (system-msg state :runner (str "trashes " (:title hosted) " from Flame-out"))
                                     (trash state side eid hosted nil))
                                 (effect-completed state side eid)))}]
    {:implementation "Credit usage restriction not enforced"
     :data {:counter {:credit 9}}
     :abilities [{:label "Take 1 [Credits] from Flame-out"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :effect (req (add-counter state side card :credit -1)
                               (gain-credits state :runner 1)
                               (system-msg state :runner "takes 1 [Credits] from Flame-out")
                               (register-events
                                 state :runner (get-card state card)
                                 [(assoc turn-end :event :runner-turn-ends)
                                  (assoc turn-end :event :corp-turn-ends)]))}
                 {:label "Take all [Credits] from Flame-out"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :effect (req (let [credits (get-counters card :credit)]
                                 (gain-credits state :runner credits)
                                 (update! state :runner (dissoc-in card [:counter :credit]))
                                 (system-msg state :runner (str "takes " credits "[Credits] from Flame-out"))
                                 (register-events
                                   state :runner (get-card state card)
                                   [(assoc turn-end :event :runner-turn-ends)
                                    (assoc turn-end :event :corp-turn-ends)])))}
                 {:label "Install a program on Flame-out"
                  :req (req (empty? (:hosted card)))
                  :cost [:click 1]
                  :prompt "Select a program in your Grip to install on Flame-out"
                  :choices {:card #(and (program? %)
                                        (in-hand? %))}
                  :async true
                  :effect (effect (update! (assoc-in (get-card state card) [:special :flame-out] (:cid target)))
                                  (runner-install eid target {:host-card card}))}
                 {:label "Host an installed program on Flame-out"
                  :req (req (empty? (:hosted card)))
                  :prompt "Select an installed program to host on Flame-out"
                  :choices {:card #(and (program? %)
                                        (installed? %))}
                  :msg (msg "host " (:title target))
                  :effect (req (->> target
                                    (get-card state)
                                    (host state side card))
                               (update! state side (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}]
     :events [{:event :card-moved
               :req (req (= (:cid target) (get-in (get-card state card) [:special :flame-out])))
               :effect (effect (update! (dissoc-in card [:special :flame-out])))}
              {:event :runner-turn-ends}
              {:event :corp-turn-ends}]
     :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                 (same-card? card (:host target))
                                                 (pos? (get-counters card :credit))))
                                  :custom-amount 1
                                  :custom (req (add-counter state side card :credit -1)
                                               (register-events
                                                 state side (get-card state card)
                                                 [(assoc turn-end :event :runner-turn-ends)
                                                  (assoc turn-end :event :corp-turn-ends)])
                                               (effect-completed state side (make-result eid 1)))
                                  :type :custom}}}))

(defcard "Flip Switch"
  {:events [{:event :pre-init-trace
             :async true
             :req (req (= :runner (:active-player @state)))
             :effect (effect (show-wait-prompt :corp "Runner to use Flip Switch")
                             (continue-ability
                               :runner
                               {:optional
                                {:prompt "Use Flip Switch to reduce base trace strength to 0?"
                                 :yes-ability {:msg "reduce the base trace strength to 0"
                                               :async true
                                               :cost [:trash]
                                               :effect (req (swap! state assoc-in [:trace :force-base] 0)
                                                            (effect-completed state side eid))}
                                 :end-effect (effect (clear-wait-prompt :corp))}}
                               card nil))}]
   :abilities [{:label "Jack out"
                :req (req (and run
                               (= :runner (:active-player @state))))
                :msg "jack out"
                :cost [:trash]
                :async true
                :effect (effect (jack-out eid))}
               {:label "Remove 1 tag"
                :req (req (and (pos? (count-tags state))
                               (= :runner (:active-player @state))))
                :msg "remove 1 tag"
                :cost [:trash]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Forger"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :constant-effects [{:type :link
                       :value 1}]
   :abilities [{:msg "avoid 1 tag"
                :label "Avoid 1 tag"
                :async true
                :cost [:trash]
                :effect (effect (tag-prevent :runner eid 1))}
               {:msg "remove 1 tag"
                :label "Remove 1 tag"
                :cost [:trash]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Friday Chip"
  (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                 :req (req (and (pos? (get-counters card :virus))
                                (pos? (count-virus-programs state))))
                 :choices {:card virus-program?}
                 :effect (req (add-counter state :runner card :virus -1)
                              (add-counter state :runner target :virus 1))}]
    {:abilities [(set-autoresolve :auto-accept "Friday Chip")]
     :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Friday Chip."))
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-trash
               :async true
               :req (req (some corp? targets))
               :effect (req (let [amt-trashed (count (filter corp? targets))
                                  sing-ab {:optional
                                           {:prompt "Place a virus counter on Friday Chip?"
                                            :autoresolve (get-autoresolve :auto-accept)
                                            :yes-ability {:effect (effect (system-msg
                                                                            :runner
                                                                            "places 1 virus counter on Friday Chip")
                                                                          (add-counter :runner card :virus 1))}}}
                                  mult-ab {:prompt "Place virus counters on Friday Chip?"
                                           :choices {:number (req amt-trashed)
                                                     :default (req amt-trashed)}
                                           :effect (effect (system-msg :runner
                                                                       (str "places "
                                                                            (quantify target "virus counter")
                                                                            " on Friday Chip"))
                                                           (add-counter :runner card :virus target))}
                                  ab (if (> amt-trashed 1) mult-ab sing-ab)]
                              (continue-ability state side ab card targets)))}]}))

(defcard "Gachapon"
  (letfn [(shuffle-end [remove-from-game shuffle-back]
            {:msg (msg "shuffle " (string/join ", " (map :title shuffle-back)) " into the stack"
                       " and remove " (string/join ", " (map :title remove-from-game)) " from the game")
             :effect (req
                       (doseq [c remove-from-game]
                         (move state side c :rfg))
                       (doseq [c shuffle-back]
                         (move state side c :deck))
                       (shuffle! state side :deck)
                       (clear-wait-prompt state :corp))})
          (shuffle-next [set-aside target to-shuffle]
            (let [set-aside (remove-once #(= % target) set-aside)
                  to-shuffle (if target
                               (concat to-shuffle [target])
                               [])
                  finished? (or (= 3 (count to-shuffle))
                                (empty? set-aside))]
              {:prompt (msg (if finished?
                              (str "Removing: " (if (not-empty set-aside)
                                                  (string/join ", " (map :title set-aside))
                                                  "nothing")
                                   "[br]Shuffling: " (if (not-empty to-shuffle)
                                                       (string/join ", " (map :title to-shuffle))
                                                       "nothing"))
                              (str "Choose " (- 3 (count to-shuffle)) " more cards to shuffle back."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Currently shuffling back: " (string/join ", " (map :title to-shuffle)))))))
               :async true
               :not-distinct true ; show cards separately
               :choices (req (if finished?
                               ["Done" "Start over"]
                               (seq set-aside)))
               :effect (req (if finished?
                              (if (= "Done" target)
                                (continue-ability state side
                                                  (shuffle-end set-aside to-shuffle)
                                                  card nil)
                                (continue-ability state side
                                                  (shuffle-next (sort-by :title (concat set-aside to-shuffle)) nil nil)
                                                  card nil))
                              (continue-ability state side
                                                (shuffle-next set-aside target to-shuffle)
                                                card nil)))}))]
    {:abilities [{:label "Install a card from the top of the stack"
                  :cost [:trash]
                  :msg "install a card from the top of the stack"
                  :async true
                  :effect (req (let [set-aside (sort-by :title (take 6 (:deck runner)))]
                                 (show-wait-prompt state :corp "Runner to resolve Gachapon")
                                 (wait-for
                                   (resolve-ability state side
                                                    {:prompt (msg "The set aside cards are: " (string/join ", " (map :title set-aside)))
                                                     :choices ["OK"]}
                                                    card nil)
                                   (continue-ability
                                     state side
                                     {:prompt "Choose a card to install"
                                      :async true
                                      :choices (req (concat
                                                      (filter #(and (or (program? %)
                                                                        (and (resource? %)
                                                                             (has-subtype? % "Virtual")))
                                                                    (can-pay? state side
                                                                              (assoc eid :source card :source-type :runner-install)
                                                                              % nil [:credit (install-cost state side % {:cost-bonus -2})]))
                                                              set-aside)
                                                      ["No action"]))
                                      :cancel-effect (effect (continue-ability (shuffle-next set-aside nil nil) card nil))
                                      :effect (req (if (= "No action" target)
                                                     (continue-ability state side (shuffle-next set-aside nil nil) card nil)
                                                     (let [to-install target
                                                           set-aside (remove-once #(= % target) set-aside)
                                                           new-eid (assoc eid :source card :source-type :runner-install)]
                                                       (wait-for (runner-install state side new-eid target {:cost-bonus -2})
                                                                 (continue-ability state side (shuffle-next set-aside nil nil) card nil)))))}
                                     card nil))))}]}))

(defcard "Gebrselassie"
  {:abilities [{:msg "host it on an installed non-AI icebreaker"
                :cost [:click 1]
                :choices {:card #(and (installed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                :effect (req (when-let [host (get-card state (:host card))]
                               (swap! state assoc :effects
                                      (reduce
                                        (fn [effects e]
                                          (conj effects
                                                (if (and (same-card? host (:card e))
                                                         (= :breaker-strength (:type e))
                                                         (:original-duration e))
                                                  (-> e
                                                      (assoc :duration (:original-duration e))
                                                      (dissoc :original-duration))
                                                  e)))
                                        []
                                        (:effects @state)))
                               (update-breaker-strength state side host))
                             (host state side target card))}]
   :events [{:event :pump-breaker
             :req (req (same-card? target (:host card)))
             :effect (req (let [last-pump (assoc (second targets)
                                                 :duration :end-of-turn
                                                 :original-duration (:duration (last (:effects @state))))]
                            (swap! state assoc :effects
                                   (->> (:effects @state)
                                        (remove #(= (:uuid last-pump) (:uuid %)))
                                        (#(conj % last-pump))
                                        (into []))))
                          (update-breaker-strength state side target))}]
   :leave-play (req (when-let [host (get-card state (:host card))]
                      (swap! state assoc :effects
                             (reduce
                               (fn [effects e]
                                 (conj effects
                                       (if (and (same-card? host (:card e))
                                                (= :breaker-strength (:type e))
                                                (:original-duration e))
                                         (-> e
                                             (assoc :duration (:original-duration e))
                                             (dissoc :original-duration))
                                         e)))
                               []
                               (:effects @state)))
                      (update-breaker-strength state side host)))})

(defcard "GPI Net Tap"
  {:implementation "Trash and jack out effect is manual"
   :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                :label "expose approached ice"
                :async true
                :effect (effect (expose eid current-ice))}]})

(defcard "Grimoire"
  {:in-play [:memory 2]
   :events [{:event :runner-install
             :silent (req true)
             :req (req (has-subtype? target "Virus"))
             :effect (effect (add-counter target :virus 1))}]})

(defcard "Heartbeat"
  {:in-play [:memory 1]
   :interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 1 damage"
                :msg "prevent 1 damage"
                :cost [:installed 1]
                :effect (effect (damage-prevent :brain 1)
                                (damage-prevent :meat 1)
                                (damage-prevent :net 1))}]})

(defcard "Hijacked Router"
  {:events [{:event :server-created
             :msg "force the Corp to lose 1 [Credits]"
             :effect (effect (lose-credits :corp 1))}
            {:event :successful-run
             :req (req (= (target-server target) :archives))
             :optional
             {:prompt "Trash Hijacked Router to force the Corp to lose 3 [Credits]?"
              :yes-ability
              {:async true
               :effect (req (system-msg state :runner "trashes Hijacked Router to force the Corp to lose 3 [Credits]")
                            (wait-for (trash state :runner card {:unpreventable true})
                                      (lose-credits state :corp 3)
                                      (effect-completed state side eid)))}}}]})

(defcard "Hippo"
  (letfn [(build-hippo-pred [outermost-ices]
            (fn [events]
              (not (empty? (filter #(true? %) (map #(and (same-card? % (first events))
                                                         (every? :broken (:subroutines (first events)))) outermost-ices))))))]
    {:events [{:event :subroutines-broken
               :optional
               {:req (req (let [servers (->> (:corp @state) :servers seq flatten)
                                outermost-ices (filter #(some? %) (map #(last (:ices %)) servers))
                                pred (build-hippo-pred outermost-ices)]
                            (and (same-card? (last run-ices) target)
                                 (every? :broken (:subroutines target))
                                 (first-event? state side :subroutines-broken pred))))
                :prompt (msg "Remove Hippo from the game to trash " (:title target) "?")
                :yes-ability
                {:async true
                 :effect (effect (system-msg (str "removes Hippo from the game to trash " (card-str state target)))
                                 (move card :rfg)
                                 (trash eid target nil))}}}]}))

(defcard "HQ Interface"
  {:in-play [:hq-access 1]})

(defcard "Keiko"
  (let [keiko-ability {:req (req (and (not (facedown? target))
                                      (has-subtype? target "Companion")
                                      (let [f #(and (not (facedown? (first %)))
                                                    (has-subtype? (first %) "Companion"))]
                                        (= 1 (+ (event-count state :runner :spent-credits-from-card f)
                                                (event-count state :runner :runner-install f))))))
                       :msg "gain 1 [Credit]"
                       :effect (effect (gain-credits :runner 1))}]
    {:in-play [:memory 2]
     :events [(assoc keiko-ability :event :spent-credits-from-card)
              (assoc keiko-ability :event :runner-install)]}))

(defcard "Knobkierie"
  {:implementation "MU usage restriction not enforced"
   :in-play [:memory 3]
   :events [{:event :successful-run
             :interactive (req true)
             :req (req (and (first-event? state :runner :successful-run)
                            (pos? (count-virus-programs state))))
             :optional {:prompt "Place a virus counter?"
                        :autoresolve (get-autoresolve :auto-add)
                        :yes-ability {:prompt "Select an installed virus program for Knobkierie to add a virus counter to"
                                      :choices {:card #(and (installed? %)
                                                            (has-subtype? % "Virus")
                                                            (program? %))}
                                      :msg (msg "place 1 virus counter on " (:title target))
                                      :effect (effect (add-counter target :virus 1))}}}]
   :abilities [(set-autoresolve :auto-add "Knobkierie")]})

(defcard "Lemuria Codecracker"
  {:abilities [{:cost [:click 1 :credit 1]
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :choices {:card installed?}
                :effect (effect (expose eid target))
                :msg "expose 1 card"}]})

(defcard "LLDS Memory Diamond"
  {:constant-effects [{:type :link
                       :value 1}]
   :in-play [:memory 1
             :hand-size 1]})

(defcard "LLDS Processor"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (has-subtype? target "Icebreaker"))
             :effect (effect (pump target 1 :end-of-turn))}]})

(defcard "Lockpick"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Decoder")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Logos"
  {:in-play [:memory 1 :hand-size 1]
   :events [{:event :agenda-scored
             :player :runner :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
             :choices (req (cancellable (:deck runner)))
             :effect (effect (trigger-event :searched-stack nil)
                             (shuffle! :deck)
                             (move target :hand))}]})

(defcard "Lucky Charm"
  {:interactions {:prevent [{:type #{:end-run}
                             :req (req (and (some #{:hq} (:successful-run runner-reg))
                                            (corp? (:card-cause target))))}]}
   :abilities [{:msg "prevent the run from ending"
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :cost [:remove-from-game]
                :effect (effect (end-run-prevent))}]})

(defcard "Mâché"
  {:abilities [{:label "Draw 1 card"
                :msg "draw 1 card"
                :cost [:power 3]
                :async true
                :effect (effect (draw :runner eid 1 nil))}]
   :events [{:event :runner-trash
             :once :per-turn
             :req (req (some #(and (:trash %)
                                   (same-card? % (:access @state)))
                             targets))
             :effect (effect (system-msg (str "places " (trash-cost state side target) " power counters on Mâché"))
                             (add-counter card :power (trash-cost state side target)))}]})

(defcard "Masterwork (v37)"
  {:in-play [:memory 1]
   :events [{:event :run
             :interactive (req true)
             :optional
             {:req (req (some #(and (hardware? %)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) card %
                                              [:credit (install-cost state side % {:cost-bonus 1})]))
                              (:hand runner)))
              :prompt "Pay 1 [Credit] to install a hardware?"
              :yes-ability {:async true
                            :prompt "Select a piece of hardware"
                            :choices
                            {:req (req (and (in-hand? target)
                                            (hardware? target)
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [:credit (install-cost state side target {:cost-bonus 1})])))}
                            :msg (msg "install " (:title target) " from the grip, paying 1 [Credit] more")
                            :effect (effect (runner-install eid target {:cost-bonus 1}))}}}
            {:event :runner-install
             :async true
             :interactive (req true)
             :req (req (and (hardware? target)
                            (first-event? state side :runner-install #(hardware? (first %)))))
             :effect (effect (draw eid 1 nil))}]})

(defcard "Māui"
  {:in-play [:memory 2]
   :recurring (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))
   :effect (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))
   :interactions {:pay-credits {:req (req (= :hq (get-in @state [:run :server 0])))
                                :type :recurring}}})

(defcard "Maw"
  {:in-play [:memory 2]
   :events [{:event :post-access-card
             :label "Trash a card from HQ"
             :async true
             :req (req (and (= 1 (get-in @state [:runner :register :no-trash-or-steal]))
                            (pos? (count (:hand corp)))
                            (not (in-discard? target))))
             :once :per-turn
             :msg "force the Corp to trash a random card from HQ"
             :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                card-seen? (same-card? target card-to-trash)
                                card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                card-to-trash)]
                            (trash state :corp eid card-to-trash nil)))}]})

(defcard "Maya"
  {:in-play [:memory 2]
   :events [{:event :post-access-card
             :async true
             :effect
             (effect
               (continue-ability
                 (let [accessed-card target]
                   {:optional
                    {:req (req (or (in-deck? accessed-card)
                                   (= [:deck] (:previous-zone accessed-card))))
                     :once :per-turn
                     :async true
                     :prompt (str "Move " (:title accessed-card) " to the bottom of R&D?")
                     :yes-ability
                     {:once :per-turn
                      :msg "move the card just accessed to the bottom of R&D"
                      :async true
                      :effect (req (move state :corp accessed-card :deck)
                                   (gain-tags state :runner eid 1))}}})
                 card nil))}]})

(defcard "MemStrips"
  {:implementation "MU usage restriction not enforced"
   :in-play [:memory 3]})

(defcard "Mind's Eye"
  {:in-play [:memory 1]
   :implementation "Power counters added automatically"
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= (target-server target) :rd))
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:async true
                :cost [:click 1 :power 3]
                :msg "access the top card of R&D"
                :effect (req (do-access state side eid [:rd] {:no-root true}))}]})

(defcard "Mirror"
  {:in-play [:memory 2]
   :events [{:event :successful-run
             :async true
             :req (req (= (target-server target) :rd))
             :effect (effect (continue-ability
                               {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                :choices {:card #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}]})

(defcard "Monolith"
  (let [mhelper
        (fn mh [n]
          {:prompt "Select a program to install"
           :choices {:req (req (and (program? target)
                                    (in-hand? target)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                              [:credit (install-cost state side target {:cost-bonus -4})])))}
           :async true
           :effect (req (wait-for (runner-install state side target {:cost-bonus -4})
                                  (continue-ability state side (when (< n 3) (mh (inc n))) card nil)))})]
    {:interactions {:prevent [{:type #{:net :brain}
                               :req (req true)}]}
     :in-play [:memory 3]
     :async true
     :effect (effect (continue-ability (mhelper 1) card nil))
     :abilities [{:msg "prevent 1 brain or net damage"
                  :cost [:trash-program-from-hand 1]
                  :effect (effect (damage-prevent :brain 1)
                                  (damage-prevent :net 1))}]}))

(defcard "Mu Safecracker"
  {:implementation "Stealth credit restriction not enforced"
   :events [{:event :successful-run
             :optional
             {:req (req (and (= (target-server target) :hq)
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 1 [Credits] to access 1 additional card?"
              :yes-ability
              {:async true
               :effect
               (effect
                 (continue-ability
                   {:eid (assoc eid :source-type :ability)
                    :async true
                    :cost [:credit 1]
                    :msg "access 1 additional card from HQ"
                    :effect (effect (access-bonus :hq 1)
                                    (effect-completed eid))}
                   card nil))}}}
            {:event :successful-run
             :optional
             {:req (req (and (= (target-server target) :rd)
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 2 [Credits] to access 1 additional card?"
              :yes-ability
              {:async true
               :effect
               (effect
                 (continue-ability
                   {:eid (assoc eid :source-type :ability)
                    :async true
                    :cost [:credit 2]
                    :msg "access 1 additional card from R&D"
                    :effect (effect (access-bonus :rd 1)
                                    (effect-completed eid))}
                   card nil))}}}]})

(defcard "Muresh Bodysuit"
  {:events [{:event :pre-damage
             :once :per-turn
             :once-key :muresh-bodysuit
             :req (req (= target :meat))
             :msg "prevent the first meat damage this turn"
             :effect (effect (damage-prevent :meat 1))}]})

(defcard "Net-Ready Eyes"
  {:effect (effect (damage eid :meat 2 {:unboostable true :card card}))
   :msg "suffer 2 meat damage"
   :events [{:event :run
             :req (req (some #(and (program? %)
                                   (has-subtype? % "Icebreaker"))
                             (all-active-installed state :runner)))
             :choices {:card #(and (installed? %)
                                   (has-subtype? % "Icebreaker"))}
             :msg (msg "give " (:title target) " +1 strength")
             :effect (effect (pump target 1 :end-of-run))}]})

(defcard "NetChip"
  {:abilities [{:async true
                :label "Install a program on NetChip"
                :req (req (empty? (:hosted card)))
                :effect (effect
                          (continue-ability
                            (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                              {:async true
                               :cost [:click 1]
                               :prompt "Select a program in your Grip to install on NetChip"
                               :choices {:card #(and (program? %)
                                                     (runner-can-install? state side % false)
                                                     (<= (:memoryunits %) n)
                                                     (in-hand? %))}
                               :msg (msg "host " (:title target))
                               :effect (effect (update! (assoc (get-card state card)
                                                               :hosted-programs
                                                               (cons (:cid target) (:hosted-programs card))))
                                               (runner-install eid target {:host-card card :no-mu true}))})
                            card nil))}
               {:async true
                :label "Host an installed program on NetChip"
                :req (req (empty? (:hosted card)))
                :effect (effect
                          (continue-ability
                            (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                              {:prompt "Select an installed program to host on NetChip"
                               :choices {:card #(and (program? %)
                                                     (<= (:memoryunits %) n)
                                                     (installed? %))}
                               :msg (msg "host " (:title target))
                               :effect (effect (host card target)
                                               (free-mu (:memoryunits target))
                                               (update! (assoc (get-card state card)
                                                               :hosted-programs
                                                               (cons (:cid target) (:hosted-programs card)))))})
                            card nil))}]
   :events [{:event :card-moved
             :req (req (some #{(:cid target)} (:hosted-programs card)))
             :effect (effect (update! (assoc card
                                             :hosted-programs
                                             (remove #(= (:cid target) %) (:hosted-programs card))))
                             (use-mu (:memoryunits target)))}]})

(defcard "Obelus"
  {:in-play [:memory 1]
   :effect (req (change-hand-size state :runner (count-tags state)))
   :leave-play (req (change-hand-size state :runner (- (count-tags state))))
   :events [{:event :run-ends
             :once :per-turn
             :req (req (and (:successful target)
                            (#{:rd :hq} (target-server target))
                            (first-event? state side :run-ends
                                          #(and (:successful (first %))
                                                (#{:rd :hq} (target-server (first %)))))))
             :msg (msg "draw " (total-cards-accessed target) " cards")
             :async true
             :effect (effect (draw eid (total-cards-accessed target) nil))}
            {:event :tags-changed
             :req (req (not= target (second targets)))
             :effect (req (let [old-total (second targets)
                                new-total (first targets)]
                            (change-hand-size state :runner (- new-total old-total))))}]})

(defcard "Omni-drive"
  {:recurring 1
   :abilities [{:async true
                :label "Install and host a program of 1[Memory Unit] or less on Omni-drive"
                :req (req (empty? (:hosted card)))
                :cost [:click 1]
                :prompt "Select a program of 1[Memory Unit] or less to install on Omni-drive from your grip"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                :msg (msg "host " (:title target))
                :effect (effect (update! (assoc (get-card state card) :Omnidrive-prog (:cid target)))
                                (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed program of 1[Memory Unit] or less on Omni-drive"
                :prompt "Select an installed program of 1[Memory Unit] or less to host on Omni-drive"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (free-mu (:memoryunits target))
                                (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}]
   :events [{:event :card-moved
             :req (req (= (:cid target) (:Omnidrive-prog (get-card state card))))
             :effect (effect (update! (dissoc card :Omnidrive-prog))
                             (use-mu (:memoryunits target)))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)
                                               (same-card? card (:host target))))
                                :type :recurring}}})

(defcard "Paragon"
  {:in-play [:memory 1]
   :events [{:event :successful-run
             :req (req (first-event? state side :successful-run))
             :async true
             :interactive (get-autoresolve :autofire (complement never?))
             :silent (get-autoresolve :autofire never?)
             :effect (effect
                       (show-wait-prompt :corp "Runner to decide if they will use Paragon")
                       (continue-ability
                         {:optional
                          {:player :runner
                           :autoresolve (get-autoresolve :auto-fire)
                           :prompt "Use Paragon?"
                           :yes-ability
                           {:msg "gain 1 [Credit] and look at the top card of Stack"
                            :async true
                            :effect (effect
                                      (gain-credits :runner 1)
                                      (continue-ability
                                        {:player :runner
                                         :optional
                                         {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                                          :yes-ability
                                          {:msg "add the top card of Stack to the bottom"
                                           :effect (effect (move :runner (first (:deck runner)) :deck)
                                                           (clear-wait-prompt :corp))}
                                          :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (system-msg "does not add the top card of the Stack to the bottom"))}}}
                                        card nil))}
                           :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                        (system-msg "does not gain 1 [Credit] and look at the top card of the Stack"))}}}
                         card nil))}]
   :abilities [(set-autoresolve :auto-fire "Paragon")]})

(defcard "Patchwork"
  (let [patchwork-ability {:once :per-turn
                           :effect (effect (update! (assoc-in card [:special :patchwork] true)))}]
    {:in-play [:memory 1]
     :interactions
     {:pay-credits
      {:req (req (and (or (= :play (:source-type eid))
                          (= :runner-install (:source-type eid)))
                      ;; We need at least one card (that is not the card played) in hand
                      (not-empty (remove (partial same-card? target) (:hand runner)))
                      ;; Patchwork wasn't used in the traditional way
                      (not (get-in card [:special :patchwork]))
                      ;; Check if Patchwork can trigger
                      (can-trigger? state side eid patchwork-ability card targets)))
       :custom-amount 2
       :custom (req (let [cost-type (str (when (= :play (:source-type eid)) "play")
                                         (when (= :runner-install (:source-type eid)) "install"))
                          patchwork card
                          targetcard target]
                      (continue-ability
                        state side
                        {:prompt (str "Trash a card to lower the " cost-type
                                      " cost of " (:title targetcard) " by 2 [Credits].")
                         :async true
                         :choices {:card #(and (in-hand? %)
                                               (runner? %)
                                               (not (same-card? % targetcard)))}
                         :msg (msg "trash " (:title target) " to lower the " cost-type " cost of "
                                   (:title targetcard) " by 2 [Credits]")
                         ; provide 2 credits
                         :effect (req (wait-for (trash state side target {:unpreventable true})
                                                (register-once state side patchwork-ability patchwork)
                                                (effect-completed state side (make-result eid 2))))
                         ; provide 0 credits
                         :cancel-effect (effect (effect-completed (make-result eid 0)))}
                        nil nil)))
       :type :custom}}}))

(defcard "Plascrete Carapace"
  {:data [:counter {:power 4}]
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:power 1]
                :msg "prevent 1 meat damage"
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "Polyhistor"
  (let [abi {:optional
             {:prompt "Draw 1 card to force the Corp to draw 1 card?"
              :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                            :async true
                            :effect (req (wait-for (draw state :runner 1 nil)
                                                   (draw state :corp eid 1 nil)))}
              :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                        (effect-completed state side eid))}}}]
    {:constant-effects [{:type :link
                       :value 1}]
     :in-play [:memory 1]
     :events [{:event :pass-ice
               :req (req (and (= (:server run) [:hq])
                              (= (:position run) 1) ; trigger when last ICE passed
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}
              {:event :run
               :req (req (and (= (:server run) [:hq])
                              (zero? (:position run)) ; trigger on unprotected HQ
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}]}))

(defcard "Prepaid VoicePAD"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :play (:source-type eid)))
                                :type :recurring}}})

(defcard "Prognostic Q-Loop"
  {:events [{:event :run
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional {:req (req (and (first-event? state side :run)
                                       (pos? (count (:deck runner)))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :player :runner
                        :prompt "Look at top 2 cards of the stack?"
                        :yes-ability {
                          :msg "look at the top 2 cards of the stack"
                          :choices ["OK"]
                          :prompt (msg "The top two cards of your Stack are "
                                  (string/join ", " (map :title (take 2 (:deck runner))))
                                  ".")}}}]
   :abilities [(set-autoresolve :auto-fire "Prognostic Q-Loop")
               {:label "Reveal and install top card of stack"
                :once :per-turn
                :cost [:credit 1]
                :req (req (pos? (count (:deck runner))))
                :msg (msg "reveal the top card of the stack: " (:title (first (:deck runner))))
                :effect
                (effect
                  (reveal (first (:deck runner)))
                  (continue-ability
                    (let [top-card (first (:deck runner))]
                      {:optional
                       {:req (req (or (program? top-card)
                                      (hardware? top-card)))
                        :prompt (msg "Install " (:title top-card) "?")
                        :yes-ability {:async true
                                      :effect (effect (runner-install eid top-card nil))}}})
                    card nil))}]})

(defcard "Public Terminal"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :play (:source-type eid))
                                               (has-subtype? target "Run")))
                                :type :recurring}}})

(defcard "Q-Coherence Chip"
  {:in-play [:memory 1]
   :events (let [e {:async true
                    :interactive (req true)
                    :req (req (some #(and (installed? %)
                                          (program? %))
                                    targets))
                    :effect (effect (system-msg "trashes Q-Coherence Chip")
                                    (trash eid card nil))}]
             [(assoc e :event :runner-trash)
              (assoc e :event :corp-trash)])})

(defcard "Qianju PT"
  {:flags {:runner-phase-12 (req true)}
   :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                :once :per-turn
                :req (req (:runner-phase-12 @state))
                :effect (effect (update! (assoc card :qianju-active true)))
                :msg "lose [Click] and avoid the first tag received until their next turn"}]
   :events [{:event :corp-turn-ends
             :effect (effect (update! (dissoc card :qianju-active)))}
            {:event :runner-turn-begins
             :req (req (:qianju-active card))
             :effect (effect (lose :click 1))}
            {:event :pre-tag
             :async true
             :req (req (:qianju-active card))
             :msg "avoid the first tag received"
             :effect (effect (update! (dissoc card :qianju-active))
                             (tag-prevent :runner eid 1))}]})

(defcard "R&D Interface"
  {:in-play [:rd-access 1]})

(defcard "Rabbit Hole"
  {:constant-effects [{:type :link
                       :value 1}]
   :optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
              :prompt "Install another Rabbit Hole?"
              :msg "install another Rabbit Hole"
              :yes-ability {:async true
                            :effect (req (trigger-event state side :searched-stack nil)
                                         (shuffle! state :runner :deck)
                                         (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                            (:deck runner))]
                                           (runner-install state side eid c nil)))}}})

(defcard "Ramujan-reliant 550 BMI"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:async true
                :label "prevent net or brain damage"
                :trash-icon true
                :req (req (not-empty (:deck runner)))
                :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose how much damage to prevent"
                                  :choices {:number (req (min n (count (:deck runner))))}
                                  :msg (msg "trash " (string/join ", " (map :title (take target (:deck runner))))
                                            " from their Stack and prevent " target " damage")
                                  :cost [:trash]
                                  :effect (effect (damage-prevent :net target)
                                                  (damage-prevent :brain target)
                                                  (mill :runner eid :runner target))}
                                 card nil)))}]})

(defcard "Recon Drone"
  ; eventmap uses reverse so we get the most recent event of each kind into map
  (letfn [(eventmap [s]
            (into {} (reverse (get s :turn-events))))]
    {:interactions {:prevent [{:type #{:net :brain :meat}
                               :req (req (:access @state))}]}
     :abilities [{:trash-icon true
                  :label "prevent damage"
                  :async true
                  :req (req (= (:cid (second (:pre-damage (eventmap @state))))
                               (:cid (first (:pre-access-card (eventmap @state))))))
                  :effect (effect (continue-ability
                                    {:prompt "Choose how much damage to prevent"
                                     :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                                 (:credit runner)))}
                                     :msg (msg "prevent " target " damage")
                                     :cost [:trash]
                                     :effect (effect (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                     (lose-credits target))}
                                    card nil))}]}))

(defcard "Record Reconstructor"
  {:events
   [{:event :successful-run
     :req (req (= (target-server target) :archives))
     :effect (effect (add-run-effect
                       {:card card
                        :replace-access
                        {:prompt "Choose one faceup card to add to the top of R&D"
                         :req (req (seq (filter :seen (:discard corp))))
                         :choices (req (filter :seen (:discard corp)))
                         :msg (msg "add " (:title target) " to the top of R&D")
                         :effect (effect (move :corp target :deck {:front true}))}}))}]})

(defcard "Reflection"
  {:in-play [:memory 1]
   :constant-effects [{:type :link
                       :value 1}]
   :events [{:event :jack-out
             :effect (req (let [card (first (shuffle (:hand corp)))]
                            (reveal state :corp card)
                            (system-msg state :runner (str  "force the Corp to reveal " (:title card) " from HQ"))))}]})

(defcard "Replicator"
  (letfn [(hardware-and-in-deck? [target runner]
            (and (hardware? target)
                 (some #(= (:title %) (:title target)) (:deck runner))))]
    {:events [{:event :runner-install
               :interactive (req (hardware-and-in-deck? target runner))
               :silent (req (not (hardware-and-in-deck? target runner)))
               :optional {:prompt "Use Replicator to add a copy?"
                          :req (req (hardware-and-in-deck? target runner))
                          :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                        :effect (effect (trigger-event :searched-stack nil)
                                                  (shuffle! :deck)
                                                  (move (some #(when (= (:title %) (:title target)) %)
                                                              (:deck runner)) :hand))}}}]}))

(defcard "Respirocytes"
  (let [ability {:once :per-turn
                 :msg "draw 1 card and add a power counter"
                 :async true
                 :effect (req (wait-for (draw state :runner 1 nil)
                                        (add-counter state side (get-card state card) :power 1)
                                        (if (= 3 (get-counters (get-card state card) :power))
                                          (do (system-msg state :runner "trashes Respirocytes as it reached 3 power counters")
                                              (trash state side eid card {:unpreventable true}))
                                          (effect-completed state side eid))))}
        event {:req (req (zero? (count (:hand runner))))
               :async true
               :effect (req (continue-ability state side ability card nil))}]
    {:implementation "Only watches trashes, playing events, and installing"
     :async true
     :effect (effect (damage eid :meat 1 {:unboostable true :card card}))
     :msg "suffer 1 meat damage"
     :events [(assoc event :event :play-event)
              (assoc event
                     :event :runner-trash
                     :req (req (and (some #(and (runner? %)
                                                (in-hand? %)) targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :corp-trash
                     :req (req (and (some #(and (runner? %)
                                                (in-hand? %)) targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :runner-install
                     :req (req (and (some #{:hand} (:previous-zone target))
                                    (zero? (count (:hand runner))))))
              {:event :runner-turn-begins
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}
              {:event :corp-turn-begins
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}]
     :abilities [ability]}))

(defcard "Rubicon Switch"
  {:abilities [{:cost [:click 1]
                :label "derez ice"
                :once :per-turn
                :async true
                :prompt "How many [Credits]?"
                :choices :credit
                :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                (continue-ability
                                  {:choices {:card #(and (ice? %)
                                                         (= :this-turn (:rezzed %))
                                                         (<= (:cost %) target))}
                                   :effect (effect (derez target))
                                   :msg (msg "derez " (:title target))}
                                  card nil))}]})

(defcard "Security Chip"
  {:abilities [{:label "Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (:title target)
                          " until the end of the run")
                :req (req (:run @state))
                :prompt "Select one non-Cloud icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                :cost [:trash]
                :effect (effect (pump target (get-link state) :end-of-run))}
               {:label "Add [Link] strength to any Cloud icebreakers until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (count targets)
                          " Cloud icebreakers until the end of the run")
                :req (req (:run @state))
                :prompt "Select any number of Cloud icebreakers"
                :choices {:max 50
                          :card #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                :cost [:trash]
                :effect (req (doseq [t targets]
                               (pump state side t (get-link state) :end-of-run)
                               (update-breaker-strength state side t)))}]})

(defcard "Security Nexus"
  {:constant-effects [{:type :link
                       :value 1}]
   :in-play [:memory 1]
   :events [{:event :encounter-ice
             :optional
             {:req (req (not-used-once? state {:once :per-turn} card))
              :prompt "Trace 5 to bypass current ice?"
              :yes-ability
              {:once :per-turn
               :msg "force the Corp to initiate a trace"
               :trace {:base 5
                       :successful {:msg "give the Runner 1 tag and end the run"
                                    :async true
                                    :effect (req (wait-for (gain-tags state :runner 1)
                                                           (end-run state side eid card)))}
                       :unsuccessful {:msg (msg "bypass " (:title current-ice))
                                      :effect (req (swap! state assoc-in [:run :bypass] true))}}}}}]})

(defcard "Severnius Stim Implant"
  (letfn [(implant-fn [srv kw]
            {:prompt "Choose at least 2 cards in your Grip to trash with Severnius Stim Implant"
             :cost [:click 1]
             :choices {:max (req (count (:hand runner)))
                       :card #(and (runner? %)
                                   (in-hand? %))}
             :msg (msg "trash " (quantify (count targets) "card")
                       " and access " (quantify (quot (count targets) 2) "additional card"))
             :async true
             :effect (req (let [bonus (quot (count targets) 2)]
                            (wait-for (trash-cards state side targets {:unpreventable true})
                                      (register-events
                                        state side card
                                        [{:event :pre-access
                                          :duration :end-of-run
                                          :silent (req true)
                                          :effect (effect (access-bonus kw bonus))}])
                                      (make-run state side eid srv nil card))))})]
    {:abilities [{:req (req (<= 2 (count (:hand runner))))
                  :label "run a server"
                  :prompt "Choose a server to run with Severnius Stim Implant"
                  :choices ["HQ" "R&D"]
                  :async true
                  :effect (effect (continue-ability (implant-fn target (if (= target "HQ") :hq :rd)) card nil))}]}))

(defcard "Şifr"
  (letfn [(index-of [pred coll]
            (some (fn [[idx item]] (if (pred item) idx))
                  (map-indexed vector coll)))
          (gather-pre-sifr-effects [sifr state side eid target targets]
            ;; This is needed because of the stupid ass rulings about how Sifr modifies
            ;; ice strength: Sifr only lowers the ice to 0 at the point it's activated,
            ;; and then other abilities (Sandburg, etc) can raise it back after, which
            ;; is DUMB, so that means we have to on the fly calculate what the strength
            ;; of the ice is at the moment Sifr would affect it.
            (if (card-flag? target :cannot-lower-strength true)
              0
              (->> (:effects @state)
                   (filter #(= :ice-strength (:type %)))
                   (filter #(if-not (:req %)
                              true
                              ((:req %) state side eid (get-card state (:card %)) (cons target targets))))
                   (#(split-at (index-of (fn [item] (same-card? sifr (:card item))) %) %))
                   (first)
                   (mapv #(if-not (fn? (:value %))
                            (:value %)
                            ((:value %) state side eid (get-card state (:card %)) (cons target targets))))
                   (reduce +)
                   (abs))))]
    {:in-play [:memory 2]
     :events [{:event :encounter-ice
               :optional
               {:req (req (not-used-once? state {:once :per-turn} card))
                :prompt "Use Şifr?"
                :yes-ability
                {:once :per-turn
                 :msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-ice) " to 0")
                 :effect (effect (lose :runner :hand-size 1)
                           (register-events
                             :runner card
                             [{:event :runner-turn-begins
                               :duration :until-runner-turn-begins
                               :effect (effect (gain :runner :hand-size 1))}])
                           (register-floating-effect
                             :runner card
                             (let [ice current-ice]
                               {:type :ice-strength
                                :duration :end-of-encounter
                                :req (req (same-card? target ice))
                                :value (req (- (+ (:strength target)
                                                  (gather-pre-sifr-effects card state side eid target (rest targets)))))})))}}}]}))

(defcard "Silencer"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Killer")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Simulchip"
  {:constant-effects [{:type :card-ability-additional-cost
                       :req (req (and (same-card? card target)
                                      (let [pred #(and (runner? (first %))
                                                       (installed? (first %))
                                                       (program? (first %)))]
                                        (zero? (+ (event-count state nil :runner-trash pred)
                                                  (event-count state nil :corp-trash pred)
                                                  (event-count state nil :game-trash pred))))))
                       :value [:program 1]}]
   :abilities [{:async true
                :label "Install a program from the heap"
                :req (req (and (not (install-locked? state side))
                               (some #(and (program? %)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side % {:cost-bonus -3})]))
                                     (:discard runner))))
                :cost [:trash]
                :msg "install a program"
                :effect
                (effect
                  (continue-ability
                    {:show-discard true
                     :choices {:req (req (and (in-discard? target)
                                              (program? target)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                        [:credit (install-cost state side target {:cost-bonus -3})])))}
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}
                    card nil))}]})

(defcard "Skulljack"
  {:effect (effect (damage eid :brain 1 {:card card}))
   :constant-effects [{:type :trash-cost
                       :value -1}]})

(defcard "Spinal Modem"
  {:in-play [:memory 1]
   :recurring 2
   :events [{:event :successful-trace
             :req (req run)
             :effect (effect (system-msg (str "suffers 1 brain damage from Spinal Modem"))
                             (damage eid :brain 1 {:card card}))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Sports Hopper"
  {:constant-effects [{:type :link
                       :value 1}]
   :abilities [{:label "Draw 3 cards"
                :msg "draw 3 cards"
                :async true
                :cost [:trash]
                :effect (effect (draw :runner eid 3 nil))}]})

(defcard "Spy Camera"
  {:abilities [{:cost [:click 1]
                :async true
                :label "Look at the top X cards of your Stack"
                :msg "look at the top X cards of their Stack and rearrange them"
                :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                             (let [n (count (filter #(= (:title %) (:title card))
                                                    (all-active-installed state :runner)))
                                   from (take n (:deck runner))]
                               (if (pos? (count from))
                                 (continue-ability state side (reorder-choice :runner :corp from '()
                                                                              (count from) from) card nil)
                                 (do (clear-wait-prompt state :corp)
                                     (effect-completed state side eid)))))}
               {:label "Look at the top card of R&D"
                :msg "look at the top card of R&D"
                :cost [:trash]
                :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck corp)))) ["OK"] {}))}]})

(defcard "Supercorridor"
  {:in-play [:memory 2 :hand-size 1]
   :events [{:event :runner-turn-ends
             :req (req (= (:credit runner) (:credit corp)))
             :interactive (get-autoresolve :autofire (complement never?))
             :silent (get-autoresolve :autofire never?)
             :async true
             :effect (req (show-wait-prompt
                            state :corp
                            "Runner to decide if they will gain credits from Supercorridor")
                          (continue-ability
                            state :runner
                            {:optional
                             {:prompt "Gain credits from Supercorridor?"
                              :player :runner
                              :autoresolve (get-autoresolve :auto-fire)
                              :yes-ability {:msg "gain 2 [Credits]"
                                            :effect (req (gain-credits state :runner 2)
                                                         (clear-wait-prompt state :corp))}
                              :no-ability {:effect (req (system-msg
                                                          state :runner
                                                          "chooses not to gain 2 [Credits] from Supercorridor")
                                                        (clear-wait-prompt state :corp))}}}
                            card nil))}]
   :abilities [(set-autoresolve :auto-fire "Supercorridor")]})

(defcard "Swift"
  {:in-play [:memory 1]
   :events [{:event :play-event
             :req (req (and (has-subtype? target "Run")
                            (first-event? state side :play-event #(has-subtype? (first %) "Run"))))
             :msg "gain a [click]"
             :effect (effect (gain :click 1))}]})

(defcard "The Gauntlet"
  {:in-play [:memory 2]
   :events [{:event :pre-access
             :req (req (= :hq target))
             :effect (req (let [broken-ice
                                (->> (run-events state side :subroutines-broken)
                                     (filter (fn [[ice broken-subs]]
                                               (and (= :hq (second (get-zone ice)))
                                                    (all-subs-broken? ice)
                                                    (get-card state ice))))
                                     (keep #(:cid (first %)))
                                     (into #{}))
                                hq-ice
                                (->> (get-in @state (concat [:corp :servers :hq :ices]))
                                     (keep :cid)
                                     (filter broken-ice))]
                            (access-bonus state :runner :hq (count hq-ice))))}]})

(defcard "The Personal Touch"
  {:hosting {:card #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
   :effect (effect (update-breaker-strength (:host card)))
   :constant-effects [{:type :breaker-strength
                       :req (req (same-card? target (:host card)))
                       :value 1}]})

(defcard "The Toolbox"
  {:constant-effects [{:type :link
                       :value 2}]
   :in-play [:memory 2]
   :recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Titanium Ribs"
  {:async true
   :effect (effect (enable-runner-damage-choice)
                   (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                   (damage eid :meat 2 {:unboostable true :card card}))
   :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-runner))
   :events [{:event :pre-resolve-damage
             :async true
             :req (req (and (pos? (last targets))
                            (runner-can-choose-damage? state)
                            (not (get-in @state [:damage :damage-replace]))))
             :effect (req (let [dtype target
                                src (second targets)
                                dmg (last targets)
                                hand (:hand runner)]
                            (show-wait-prompt state :corp "Runner to use Titanium Ribs to choose cards to be trashed")
                            (continue-ability
                              state :runner
                              (if (< (count hand) dmg)
                                {:async true
                                 :effect (req (clear-wait-prompt state :corp)
                                              (chosen-damage state :runner hand)
                                              (effect-completed state side eid))}
                                {:prompt (msg "Select " dmg " cards to trash for the " (name dtype) " damage")
                                 :choices {:max dmg
                                           :all true
                                           :card #(and (in-hand? %)
                                                       (runner? %))}
                                 :msg (msg "trash " (string/join ", " (map :title targets)))
                                 :effect (req (clear-wait-prompt state :corp)
                                              (chosen-damage state :runner targets)
                                              (effect-completed state side eid))})
                              card nil)))}]})

(defcard "Top Hat"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req (and (= (target-server target) :rd)
                            (not= (:max-access run) 0)))
             :async true
             :effect
             (effect
               (continue-ability
                 {:optional
                  {:prompt "Use Top Hat to access a single card?"
                   :yes-ability
                   {:req (req (pos? (count (:deck corp))))
                    :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
                    :choices (map str (take (count (:deck corp)) (range 1 6)))
                    :msg (msg "only access the card at position " target " of R&D")
                    :effect (effect
                              (add-run-effect
                                (let [t target]
                                  {:card card
                                   :replace-access
                                   {:mandatory true
                                    :async true
                                    :effect (req (if (not (get-only-card-to-access state))
                                                   (access-card state side eid (nth (:deck corp) (dec (str->int t))) "an unseen card")
                                                   (effect-completed state side eid)))}})))}}}
                 card nil))}]})

(defcard "Turntable"
  {:in-play [:memory 1]
   :events [{:event :agenda-stolen
             :interactive (req true)
             :req (req (not (empty? (:scored corp))))
             :async true
             :effect (effect
                       (continue-ability
                         (let [stolen target]
                           {:optional
                            {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                             :yes-ability
                             {:async true
                              :prompt (str "Select a scored Corp agenda to swap with " (:title stolen))
                              :choices {:card #(in-corp-scored? state side %)}
                              :effect (effect (swap-agendas target stolen)
                                              (system-msg (str "uses Turntable to swap "
                                                               (:title stolen) " for " (:title target)))
                                              (effect-completed eid))}}})
                         card targets))}]})

(defcard "Ubax"
  (let [ability {:req (req (:runner-phase-12 @state))
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1 nil))}]
    {:in-play [:memory 1]
     :flags {:runner-turn-draw true
             :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                       (cons (get-in @state [:runner :identity])
                                                             (all-active-installed state :runner))))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Unregistered S&W '35"
  {:abilities
   [{:cost [:click 2]
     :req (req (and (some #{:hq} (:successful-run runner-reg))
                    (seq (filter
                           #(and (rezzed? %)
                                 (installed? %)
                                 (or (has-subtype? % "Bioroid")
                                     (has-subtype? % "Clone")
                                     (has-subtype? % "Executive")
                                     (has-subtype? % "Sysop")))
                           (all-active-installed state :corp)))))
     :label "trash a Bioroid, Clone, Executive or Sysop"
     :prompt "Select a Bioroid, Clone, Executive, or Sysop to trash"
     :choices {:card #(and (rezzed? %)
                           (installed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop")))}
     :async true
     :msg (msg "trash " (:title target))
     :effect (effect (trash eid target nil))}]})

(defcard "Vigil"
  (let [ability {:req (req (and (:runner-phase-12 @state) (= (count (:hand corp)) (hand-size state :corp))))
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1 nil))}]
    {:in-play [:memory 1]
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Window"
  {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                :effect (effect (move (last (:deck runner)) :hand))}]})

(defcard "Zamba"
  {:implementation "Credit gain is automatic"
   :in-play [:memory 2]
   :events [{:event :expose
             :effect (effect (gain-credits :runner 1))
             :msg "gain 1 [Credits]"}]})

(defcard "Zer0"
  {:abilities [{:cost [:click 1 :net 1]
                :once :per-turn
                :msg "gain 1 [Credits] and draw 2 cards"
                :async true
                :effect (effect (gain-credits 1)
                                (draw eid 2 nil))}]})
