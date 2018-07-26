(in-ns 'game.cards.operations)

(def card-definition-an-offer-you-can-t-refuse
  {"An Offer You Can't Refuse"
   {:async true
    :prompt "Choose a server" :choices ["Archives" "R&D" "HQ"]
    :effect (req (let [serv target]
                   (show-wait-prompt state :corp (str "Runner to decide on running " target))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :runner
                       :yes-ability {:msg (msg "let the Runner make a run on " serv)
                                     :effect (effect (clear-wait-prompt :corp)
                                                     (game.core/run eid serv nil card))}
                       :no-ability {:async true
                                    :effect (req (clear-wait-prompt state :corp)
                                                 (as-agenda state :corp eid (some #(when (= (:cid card) (:cid %)) %) (:discard corp)) 1))
                                    :msg "add it to their score area as an agenda worth 1 agenda point"}}}
                     card nil)))}})
