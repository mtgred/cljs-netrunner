(ns game-test.core
  (:require [game.core :as core]
            [game.utils :as utils :refer [make-cid]]
            [jinteki.cards :refer [all-cards]]
            [hawk.core :as hawk]
            [clojure.test :refer :all]))

;; Card information and definitions
(defn load-cards []
  (->> (clojure.java.io/file "data/cards")
       file-seq
       (filter #(.isFile %))
       (map slurp)
       (map read-string)
       merge))

(defn load-all-cards []
  (when (empty? @all-cards)
    (reset! all-cards (into {} (map (juxt :title identity)
                                    (map #(assoc % :cid (make-cid)) (load-cards)))))
    (core/reset-card-defs)))
(load-all-cards)

(hawk/watch! [{:paths ["src/clj/game/cards"]
               :filter hawk/file?
               :handler (fn [ctx e]
                          (load-file (-> e :file str)))}])

;; General utilities necessary for starting a new game
(defn find-card
  "Return a card with given title from given sequence"
  [title from]
  (some #(when (= (:title %) title) %) from))

(defn starting-hand
  "Moves all cards in the player's hand to their draw pile, then moves the specified card names
  back into the player's hand."
  [state side cards]
  (doseq [c (get-in @state [side :hand])]
    (core/move state side c :deck))
  (doseq [ctitle cards]
    (core/move state side (find-card ctitle (get-in @state [side :deck])) :hand)))

(defn take-credits
  "Take credits for n clicks, or if no n given, for all remaining clicks of a side.
  If all clicks are used up, end turn and start the opponent's turn."
  ([state side] (take-credits state side nil))
  ([state side n]
   (let  [remaining-clicks (get-in @state [side :click])
          n (or n remaining-clicks)
          other (if (= side :corp) :runner :corp)]
     (dotimes [i n] (core/click-credit state side nil))
     (if (= (get-in @state [side :click]) 0)
       (do (core/end-turn state side nil)
           (core/start-turn state other nil))))))


;; Deck construction helpers
(defn qty [card amt]
  (when (pos? amt)
    (repeat amt card)))

(defn card-vec->card-map
  [[card amt]]
  (let [loaded-card (if (string? card) (@all-cards card) card)]
    (when-not loaded-card
      (throw (Exception. (str card " not found in @all-cards"))))
    {:card loaded-card
     :qty amt}))

(defn transform
  [cards]
  (->> cards
       flatten
       (filter string?)
       frequencies
       (map card-vec->card-map)
       seq))

(defn make-decks
  [{:keys [corp runner]}]
  {:corp {:deck (or (transform (conj (:deck corp)
                                     (:hand corp)
                                     (:discard corp)))
                    (transform (qty "Hedge Fund" 3)))
          :hand (flatten (:hand corp))
          :discard (flatten (:discard corp))
          :identity (@all-cards
                      (or (:id corp)
                          "Custom Biotics: Engineered for Success"))
          :credits (:credits corp)}
   :runner {:deck (or (transform (conj (:deck runner)
                                       (:hand runner)
                                       (:discard runner)))
                      (transform (qty "Sure Gamble" 3)))
            :hand (flatten (:hand runner))
            :discard (flatten (:discard runner))
            :identity (@all-cards
                        (or (:id runner)
                            "The Professor: Keeper of Knowledge"))
            :credits (:credits runner)}})

(defn new-game
  "Init a new game using given corp and runner. Keep starting hands (no mulligan) and start Corp's turn."
  ([] (new-game nil nil))
  ([args]
   (if (or (contains? args :corp)
           (contains? args :runner))
     (new-game args nil)
     (new-game nil args)))
  ([players {:keys [mulligan start-as dont-start-turn dont-start-game] :as args}]
   (let [{:keys [corp runner]} (make-decks players)
         state (core/init-game
                 {:gameid 1
                  :players [{:side "Corp"
                             :user "player1"
                             :deck {:identity (:identity corp)
                                    :cards (:deck corp)}}
                            {:side "Runner"
                             :user "player2"
                             :deck {:identity (:identity runner)
                                    :cards (:deck runner)}}]})]
     (when-not dont-start-game
       (if (#{:both :corp} mulligan)
         (core/resolve-prompt state :corp {:choice "Mulligan"})
         (core/resolve-prompt state :corp {:choice "Keep"}))
       (if (#{:both :runner} mulligan)
         (core/resolve-prompt state :runner {:choice "Mulligan"})
         (core/resolve-prompt state :runner {:choice "Keep"}))
       (when-not dont-start-turn (core/start-turn state :corp nil))
       (when (= start-as :runner) (take-credits state :corp)))
     ;; Gotta move cards where they need to go
     (doseq [side [:corp :runner]]
       (let [side-map (if (= :corp side) corp runner)]
         (when-let [hand (seq (:hand side-map))]
           (starting-hand state side hand))
         (when (seq (:discard side-map))
           (doseq [card (:discard side-map)]
             (core/move state side
                        (find-card (:card card) (get-in @state [side :deck])) :discard)))
         (when (:credits side-map)
           (swap! state assoc-in [side :credit] (:credits side-map)))))
     ;; These are side independent so they happen ouside the loop
     (when (:bad-pub corp)
       (swap! state assoc-in [:corp :bad-publicity] (:bad-pub corp)))
     (when (:tags runner)
       (swap! state assoc-in [:runner :tag] (:tags runner)))
     state)))

;;; Card related functions
(defn card-ability
  "Trigger a card's ability with its 0-based index. Refreshes the card argument before
  triggering the ability."
  ([state side card ability] (card-ability state side card ability nil))
  ([state side card ability targets]
   (core/play-ability state side {:card (core/get-card state card)
                                  :ability ability
                                  :targets targets})))

(defn card-subroutine
  "Trigger a piece of ice's subroutine with the 0-based index."
  ([state side card ability] (card-subroutine state side card ability nil))
  ([state side card ability targets]
   (core/play-subroutine state side {:card (core/get-card state card)
                                     :subroutine ability
                                     :targets targets})))

(defn card-side-ability
  ([state side card ability] (card-side-ability state side card ability nil))
  ([state side card ability targets]
   (let [ab {:card (core/get-card state card)
             :ability ability
             :targets targets}]
     (if (= :corp side)
       (core/play-corp-ability state side ab)
       (core/play-runner-ability state side ab)))))

(defn get-ice
  "Get installed ice protecting server by position. If no pos, get all ice on the server."
  ([state server]
   (get-in @state [:corp :servers server :ices]))
  ([state server pos]
   (get-in @state [:corp :servers server :ices pos])))

(defn get-content
  "Get card in a server by position. If no pos, get all cards in the server."
  ([state server]
   (get-in @state [:corp :servers server :content]))
  ([state server pos]
   (get-in @state [:corp :servers server :content pos])))

(defn get-program
  "Get non-hosted program by position. If no pos, get all installed programs."
  ([state] (get-in @state [:runner :rig :program]))
  ([state pos]
   (get-in @state [:runner :rig :program pos])))

(defn get-hardware
  "Get hardware by position. If no pos, get all installed hardware."
  ([state] (get-in @state [:runner :rig :hardware]))
  ([state pos]
   (get-in @state [:runner :rig :hardware pos])))

(defn get-resource
  "Get non-hosted resource by position. If no pos, get all installed resources."
  ([state] (get-in @state [:runner :rig :resource]))
  ([state pos]
   (get-in @state [:runner :rig :resource pos])))

(defn get-runner-facedown
  "Get non-hosted runner facedown by position. If no pos, get all runner facedown installed cards."
  ([state] (get-in @state [:runner :rig :facedown]))
  ([state pos]
   (get-in @state [:runner :rig :facedown pos])))

(defn get-discarded
  "Get discarded card by position. If no pos, selects most recently discarded card."
  ([state side] (get-discarded state side (-> @state side :discard count dec)))
  ([state side pos]
   (get-in @state [side :discard pos])))

(defn get-scored
  "Get a card from the score area. Can find by name or index.
  If no index or name provided, gets all scored cards."
  ([state side] (get-in @state [side :scored]))
  ([state side x]
   (if (number? x)
     ;; Find by index
     (get-in @state [side :scored x])
     ;; Find by name
     (when (string? x)
       (find-card x (get-in @state [side :scored]))))))

(def get-counters utils/get-counters)

(defn play-from-hand
  "Play a card from hand based on its title. If installing a Corp card, also indicate
  the server to install into with a string."
  ([state side title] (play-from-hand state side title nil))
  ([state side title server]
   (core/play state side {:card (find-card title (get-in @state [side :hand]))
                          :server server})))


;;; Run functions
(defn play-run-event
  "Play a run event with a replace-access effect on an unprotected server.
  Advances the run timings to the point where replace-access occurs."
  ([state card server] (play-run-event state card server true))
  ([state card server show-prompt]
   (let [card (if (map? card) card (find-card card (get-in @state [:runner :hand])))]
     (core/play state :runner {:card card})
     (is (= [server] (get-in @state [:run :server])) "Correct server is run")
     (is (get-in @state [:run :run-effect]) "There is a run-effect")
     (core/no-action state :corp nil)
     (core/successful-run state :runner nil)
     (if show-prompt
       (is (get-in @state [:runner :prompt]) "A prompt is shown")
       (is (not (get-in @state [:runner :prompt])) "A prompt is not shown"))
     (is (get-in @state [:run :successful]) "Run is marked successful"))))

(defn run-on
  "Start run on specified server."
  [state server]
  (core/click-run state :runner {:server server}))

(defn run-continue
  "No action from corp and continue for runner to proceed in current run."
  [state]
  (core/no-action state :corp nil)
  (core/continue state :runner nil))

(defn run-phase-43
  "Ask for triggered abilities phase 4.3"
  [state]
  (core/corp-phase-43 state :corp nil)
  (core/successful-run state :runner nil))

(defn run-successful
  "No action from corp and successful run for runner."
  [state]
  (core/no-action state :corp nil)
  (core/successful-run state :runner nil))

(defn run-jack-out
  "Jacks out in run."
  [state]
  (core/jack-out state :runner nil))

(defn run-empty-server
  "Make a successful run on specified server, assumes no ice in place."
  [state server]
  (run-on state server)
  (run-successful state))


;;; Misc functions
(defn score-agenda
  "Take clicks and credits needed to advance and score the given agenda."
  ([state _ card]
   (let [title (:title card)
         advancementcost (:advancementcost card)]
     (core/gain state :corp :click advancementcost :credit advancementcost)
     (dotimes [n advancementcost]
       (core/advance state :corp {:card (core/get-card state card)}))
     (is (= advancementcost (get-counters (core/get-card state card) :advancement)))
     (core/score state :corp {:card (core/get-card state card)})
     (is (find-card title (get-scored state :corp))))))

(defn advance
  "Advance the given card."
  ([state card] (advance state card 1))
  ([state card n]
   (dotimes [_ n]
     (core/advance state :corp {:card (core/get-card state card)}))))

(defn last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log last :text))))

(defn second-last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log butlast last :text))))

(defn trash-from-hand
  "Trash specified card from hand of specified side"
  [state side title]
  (core/trash state side (find-card title (get-in @state [side :hand]))))

(defn trash-resource
  "Trash specified card from rig of the runner"
  [state title]
  (core/trash state :runner (find-card title (get-in @state [:runner :rig :resource]))))

(defn accessing
  "Checks to see if the runner has a prompt accessing the given card title"
  [state title]
  (= title (-> @state :runner :prompt first :card :title)))

(defn play-and-score
  "Play an agenda from the hand into a new server and score it. Unlike score-agenda, spends a click."
  [state title]
  (play-from-hand state :corp title "New remote")
  (score-agenda state :corp (get-content state (keyword (str "remote" (:rid @state))) 0)))
