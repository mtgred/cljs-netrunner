(ns game.core.diffs
  (:require [game.core.card :refer [agenda? installed? can-be-advanced? on-host? ice? program? upgrade? asset? rezzed?]]
            [game.core.flags :refer [card-is-public? can-score?]]
            [game.utils :refer [dissoc-in]]
            [differ.core :as differ]))

(defn action-list
  [state card]
  (cond-> []
    ;; advance
    (or (and (agenda? card)
             (or (installed? card)
                 (on-host? card)))
        (can-be-advanced? card))
    (conj "advance")
    ;; score
    (and (agenda? card)
         (can-score? state :corp card))
    (conj "score")
    ;; trash
    (or (ice? card)
        (program? card))
    (conj "trash")
    ;; rez
    (and (or (asset? card)
             (ice? card)
             (upgrade? card))
         (not (rezzed? card)))
    (conj "rez")
    ;; derez
    (and (or (asset? card)
             (ice? card)
             (upgrade? card))
         (rezzed? card))
    (conj "derez")))

(defn card-summary
  [state card]
  (-> card
      ; (select-keys [:cid :side :title :zone :counter :advance-counter :new :icon])
      (assoc :hosted (mapv #(card-summary state %) (:hosted card))
             :actions (action-list state card))))

(defn private-card
  "Returns only the public information of a given card when it's in a private state,
  for example, when it's facedown or in the hand"
  [card]
  (select-keys card [:zone :cid :side :new :host :counter :advance-counter :hosted :icon]))

(defn- strip [state]
  (-> state
      (dissoc :eid :events :turn-events :per-turn :prevent :damage :effect-completed :click-state :turn-state :history)
      (update-in [:corp :register] select-keys [:spent-click])
      (update-in [:runner :register] select-keys [:spent-click])
      (dissoc-in [:corp :register-last-turn])
      (dissoc-in [:runner :register-last-turn])
      (dissoc-in [:run :current-ice])
      (dissoc-in [:run :events])))

(defn strip-for-replay [state]
  (-> state
      (strip)
      (dissoc-in [:runner :user :isadmin])
      (dissoc-in [:runner :user :options :blocked-users])
      (dissoc-in [:runner :user :stats])
      (dissoc-in [:corp :user :isadmin])
      (dissoc-in [:corp :user :options :blocked-users])
      (dissoc-in [:corp :user :stats])))

(defn- private-card-vector [state side cards]
  (mapv (fn [card]
          (cond
            (not (card-is-public? state side card)) (private-card card)
            (:hosted card) (update card :hosted #(private-card-vector state side %))
            :else card))
        cards))

(defn- make-opponent-runner [state]
  (-> (:runner @state)
      (dissoc :runnable-list)
      (update :hand #(private-card-vector state :runner %))
      (update :discard #(private-card-vector state :runner %))
      (assoc :deck []
             :deck-count (count (get-in @state [:runner :deck]))
             :hand-count (count (get-in @state [:runner :hand])))
      (update :hand #(if (get-in @state [:runner :openhand]) % []))
      (update-in [:rig :facedown] #(private-card-vector state :runner %))
      (update-in [:rig :resource] #(private-card-vector state :runner %))))

(defn- make-opponent-corp [state]
  (let [zones (concat [[:discard]]
                      (for [server (keys (:servers (:corp @state)))] [:servers server :ices])
                      (for [server (keys (:servers (:corp @state)))] [:servers server :content]))
        corp (-> (:corp @state)
                 (dissoc :install-list)
                 (assoc :hand []
                        :deck []
                        :deck-count (count (get-in @state [:corp :deck]))
                        :hand-count (count (get-in @state [:corp :hand]))))]
    (loop [s corp
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :corp %)) (rest z))))))

(defn- make-player-runner [state]
  (let [view-deck (get-in @state [:runner :view-deck])
        deck (get-in @state [:runner :deck])
        hand (get-in @state [:runner :hand])]
    (-> (:runner @state)
        (dissoc :runnable-list)
        (assoc :hand (mapv (partial card-summary state) hand)
               :deck (if view-deck deck [])
               :deck-count (count deck)
               :hand-count (count hand))
        (update-in [:rig :hardware] #(mapv (partial card-summary state) %))
        (update-in [:rig :program] #(mapv (partial card-summary state) %))
        (update-in [:rig :resource] #(mapv (partial card-summary state) %)))))

(defn- make-player-corp [state]
  (let [view-deck (get-in @state [:corp :view-deck])
        deck (get-in @state [:corp :deck])
        hand (get-in @state [:corp :hand])]
    (let [zones (mapcat #(do [[:servers % :content] [:servers % :ices]]) (keys (:servers (:corp @state))))
          corp (-> (:corp @state)
                   (assoc :hand (mapv (partial card-summary state) hand)
                          :deck (if view-deck deck [])
                          :deck-count (count deck)
                          :hand-count (count hand)))]
      (loop [s corp
             z zones]
        (if (empty? z)
          s
          (recur (update-in s (first z) #(mapv (partial card-summary state) %))
                 (next z)))))))

(defn- private-states
  "Generates privatized states for the Corp, Runner, any spectators, and the history from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  (let [corp-player (make-player-corp state)
        runner-player (make-player-runner state)
        corp-opponent (make-opponent-corp state)
        runner-opponent (make-opponent-runner state)]
    ;; corp, runner, spectator, history
    [(assoc @state :runner runner-opponent :corp corp-player)
     (assoc @state :corp corp-opponent :runner runner-player)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :corp corp-player :runner runner-player)
       (assoc @state :corp corp-opponent :runner runner-opponent))
     @state]))

(defn public-states [state]
  (let [[new-corp new-runner new-spect new-hist] (private-states state)]
    {:runner-state (strip new-runner)
     :corp-state   (strip new-corp)
     :spect-state  (strip new-spect)
     :hist-state   (strip-for-replay new-hist)}))

(defn public-diffs [old-state new-state]
  (let [[old-corp old-runner old-spect old-hist] (when old-state (private-states (atom old-state)))
        [new-corp new-runner new-spect new-hist] (private-states new-state)
        runner-diff (differ/diff (strip old-runner) (strip new-runner))
        corp-diff (differ/diff (strip old-corp) (strip new-corp))
        spect-diff (differ/diff (strip old-spect) (strip new-spect))
        hist-diff (differ/diff (strip-for-replay old-hist) (strip-for-replay new-hist))]
    {:runner-diff runner-diff
     :corp-diff   corp-diff
     :spect-diff  spect-diff
     :hist-diff   hist-diff}))
