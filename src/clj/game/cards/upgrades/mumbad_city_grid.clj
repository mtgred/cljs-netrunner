(in-ns 'game.cards.upgrades)

(def card-definition-mumbad-city-grid
  {"Mumbad City Grid"
   {:abilities [{:req (req (let [num-ice (count run-ices)]
                             (and this-server
                                  (>= num-ice 2)
                                  (< (:position run 0) num-ice))))
                 :label "Swap the ICE just passed with another piece of ICE protecting this server"
                 :effect (req (let [passed-ice (nth (get-in @state (vec (concat [:corp :servers] (:server run) [:ices])))
                                                                                (:position run))
                                    ice-zone (:zone passed-ice)]
                                 (resolve-ability state :corp
                                   {:prompt (msg "Select a piece of ICE to swap with " (:title passed-ice))
                                    :choices {:req #(and (= ice-zone (:zone %)) (ice? %))}
                                    :effect (req (let [fndx (ice-index state passed-ice)
                                                       sndx (ice-index state target)
                                                       fnew (assoc passed-ice :zone (:zone target))
                                                       snew (assoc target :zone (:zone passed-ice))]
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % fndx snew))
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % sndx fnew))
                                                   (update-ice-strength state side fnew)
                                                   (update-ice-strength state side snew)
                                                   (system-msg state side (str "uses Mumbad City Grid to swap "
                                                                               (card-str state passed-ice)
                                                                               " with " (card-str state target)))))}
                                                  card nil)))}]}})
