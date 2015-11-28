(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [split split-lines join escape]]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.cardbrowser :refer [cards-channel image-url card-view] :as cb]
            [netrunner.ajax :refer [POST GET]]))

(def select-channel (chan))
(def zoom-channel (chan))

(defn identical-cards? [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn found? [query cards]
  (some #(if (= (.toLowerCase (:title %)) query) %) cards))

(defn search [query cards]
  (filter #(if (= (.indexOf (.toLowerCase (:title %)) query) -1) false true) cards))

(defn alt-art? [card]
  (or (get-in @app-state [:user :special])
      (not= "Alternates" (:setname card))))

(defn lookup [side query]
  (let [q (.toLowerCase query)
        cards (filter #(and (= (:side %) side) (alt-art? %))
                      (:cards @app-state))]
    (if-let [card (some #(when (= (-> % :title .toLowerCase) q) %) cards)]
      card
      (loop [i 2 matches cards]
        (let [subquery (subs q 0 i)]
         (cond (zero? (count matches)) query
               (or (= (count matches) 1) (identical-cards? matches)) (first matches)
               (found? subquery matches) (found? subquery matches)
               (<= i (count query)) (recur (inc i) (search subquery matches))
               :else query))))))

(defn parse-line [side line]
  (let [tokens (split line " ")
        qty (js/parseInt (first tokens))
        cardname (join " " (rest tokens))]
    (when-not (js/isNaN qty)
      {:qty (min qty 3) :card (lookup side cardname)})))

(defn parse-deck [side deck]
  (reduce #(if-let [card (parse-line side %2)] (conj %1 card) %1) [] (split-lines deck)))

(defn allowed? [card {:keys [side faction title] :as identity}]
  (and (not= (:type card) "Identity")
       (= (:side card) side)
       (or (not= (:type card) "Agenda")
           (= (:faction card) "Neutral")
           (= (:faction card) faction))
       (or (not= title "Custom Biotics: Engineered for Success")
           (not= (:faction card) "Jinteki"))))

(defn load-decks [decks]
  (swap! app-state assoc :decks decks)
  (put! select-channel (first (sort-by :date > decks))))

(defn process-decks [decks]
  (for [deck decks]
    (let [cards (map #(str (:qty %) " " (:card %)) (:cards deck))]
      (assoc deck :cards (parse-deck (get-in deck [:identity :side]) (join "\n" cards))))))

(go (let [cards (<! cards-channel)
          decks (process-decks (:json (<! (GET (str "/data/decks")))))]
      (load-decks decks)
      (>! cards-channel cards)))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(defn side-identities [side]
  (->> (:cards @app-state)
       (filter #(and (= (:side %) side)
                     (= (:type %) "Identity")
                     (alt-art? %)))
       (distinct-by :title)))

(defn get-card [title]
  (some #(when (and (= (:title %) title) (alt-art? %)) %)
        (:cards @app-state)))

(defn deck->str [owner]
  (let [cards (om/get-state owner [:deck :cards])
        str (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) "\n") "" cards)]
    (om/set-state! owner :deck-edit str)))

(defn influence [deck]
  (let [identity (:identity deck)
        cards (:cards deck)
        inf (reduce #(let [card (:card %2)]
                       (if (= (:faction card) (:faction identity))
                         %1
                         (+ %1 (* (:qty %2) (:factioncost card)))))
                    0 (:cards deck))]
    (if (= (:title identity) "The Professor: Keeper of Knowledge")
      (- inf (reduce #(+ %1 (get-in %2 [:card :factioncost])) 0
                     (filter #(let [card (:card %)]
                                (and (= (:type card) "Program")
                                     (not= (:faction card) (:faction identity))))
                             cards)))
      inf)))

(defn card-count [cards]
  (reduce #(+ %1 (:qty %2)) 0 cards))

(defn min-agenda-points [deck]
  (let [size (max (card-count (:cards deck)) (get-in deck [:identity :minimumdecksize]))]
    (+ 2 (* 2 (quot size 5)))))

(defn agenda-points [{:keys [cards]}]
  (reduce #(if-let [point (get-in %2 [:card :agendapoints])]
             (+ (* point (:qty %2)) %1) %1) 0 cards))

(defn valid? [{:keys [identity cards] :as deck}]
  (and (>= (card-count cards) (:minimumdecksize identity))
       (<= (influence deck) (:influencelimit identity))
       (every? #(and (allowed? (:card %) identity)
                     (<= (:qty %) (or (get-in % [:card :limited]) 3))) cards)
       (or (= (:side identity) "Runner")
           (let [min (min-agenda-points deck)]
             (<= min (agenda-points deck) (inc min))))))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (deck->str owner)
  (-> owner (om/get-node "viewport") js/$ (.addClass "edit"))
  (try (js/ga "send" "event" "deckbuilder" "edit") (catch js/Error e))
  (go (<! (timeout 500))
      (-> owner (om/get-node "deckname") js/$ .select)))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:name "New deck" :cards [] :identity (-> side side-identities first)})
  (try (js/ga "send" "event" "deckbuilder" "new" side) (catch js/Error e))
  (edit-deck owner))

(defn end-edit [owner]
  (om/set-state! owner :edit false)
  (om/set-state! owner :query "")
  (-> owner (om/get-node "viewport") js/$ (.removeClass "edit")))

(defn save-deck [cursor owner]
  (authenticated
   (fn [user]
     (end-edit owner)
     (let [deck (assoc (om/get-state owner :deck) :date (.toJSON (js/Date.)))
           decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))
           cards (for [card (:cards deck) :when (get-in card [:card :title])]
                   {:qty (:qty card) :card (get-in card [:card :title])})
           data (assoc deck :cards cards)]
       (try (js/ga "send" "event" "deckbuilder" "save") (catch js/Error e))
       (go (let [new-id (get-in (<! (POST "/data/decks/" data :json)) [:json :_id])
                 new-deck (if (:_id deck) deck (assoc deck :_id new-id))]
             (om/update! cursor :decks (conj decks new-deck))
             (om/set-state! owner :deck new-deck)))))))

(defn match [identity query]
  (if (empty? query)
    []
    (let [cards (->> (:cards @app-state)
                     (filter #(and (allowed? % identity)
                                   (not= "Special" (:setname %))
                                   (alt-art? %)))
                     (distinct-by :title))]
      (take 10 (filter #(not= (.indexOf (.toLowerCase (:title %)) (.toLowerCase query)) -1) cards)))))

(defn handle-edit [owner]
  (let [text (.-value (om/get-node owner "deck-edit"))]
    (om/set-state! owner :deck-edit text)
    (om/set-state! owner [:deck :cards] (parse-deck (om/get-state owner [:deck :identity :side]) text))))

(defn handle-delete [cursor owner]
  (authenticated
   (fn [user]
     (let [deck (om/get-state owner :deck)]
       (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e))
       (go (let [response (<! (POST "/data/decks/delete" deck :json))]))
       (om/transact! cursor :decks (fn [ds] (remove #(= deck %) ds)))
       (om/set-state! owner :deck (first (sort-by :date > (:decks @cursor))))))))

(defn html-escape [st]
  (escape st {\< "&lt;" \> "&gt;" \& "&amp;" \" "#034;"}))

(defn not-alternate [card]
  (if (= (:setname card) "Alternates")
    (some #(when (and (not= (:setname %) "Alternates") (= (:title %) (:title card))) %)
          (:cards @app-state))
    card))

(defn octgn-link [owner]
  (let [deck (om/get-state owner :deck)
        identity (not-alternate (:identity deck))
        id "bc0f047c-01b1-427f-a439-d451eda"
        xml (str
             "<deck game=\"0f38e453-26df-4c04-9d67-6d43de939c77\"><section name=\"Identity\"><card qty=\"1\" id=\""
             id (:code identity) "\">" (:title identity) "</card></section>"
             "<section name=\"R&amp;D / Stack\">"
             (apply str (for [c (:cards deck) :when (get-in c [:card :title])]
                          (str "<card qty=\"" (:qty c) "\" id=\"" id (get-in c [:card :code]) "\">"
                               (html-escape (get-in c [:card :title])) "</card>")))
             "</section></deck>")
        blob (js/Blob. (clj->js [xml]) #js {:type "application/download"})]
    (.createObjectURL js/URL blob)))

(defn handle-keydown [owner event]
  (let [selected (om/get-state owner :selected)
        matches (om/get-state owner :matches)]
    (case (.-keyCode event)
      38 (when (> selected 0)
           (om/update-state! owner :selected dec))
      40 (when (< selected (dec (count matches)))
           (om/update-state! owner :selected inc))
      (9 13) (when-not (= (om/get-state owner :query) (:title (first matches)))
               (.preventDefault event)
               (-> ".deckedit .qty" js/$ .select)
               (om/set-state! owner :query (:title (nth matches selected))))
      (om/set-state! owner :selected 0))))

(defn handle-add [owner event]
  (.preventDefault event)
  (let [qty (js/parseInt (om/get-state owner :quantity))]
    (if (js/isNaN qty)
      (om/set-state! owner :quantity 3)
      (do (put! (om/get-state owner :edit-channel)
                {:qty qty
                 :card (nth (om/get-state owner :matches) (om/get-state owner :selected))})
          (om/set-state! owner :quantity 3)
          (om/set-state! owner :query "")
          (-> ".deckedit .lookup" js/$ .select)))))

(defn card-lookup [{:keys [cards]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:query ""
       :matches []
       :quantity 3
       :selected 0})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:p
        [:h3 "Add cards"]
        [:form.card-search {:on-submit #(handle-add owner %)}
         [:input.lookup {:type "text" :placeholder "Card name" :value (:query state)
                         :on-change #(om/set-state! owner :query (.. % -target -value))
                         :on-key-down #(handle-keydown owner %)}]
         " x "
         [:input.qty {:type "text" :value (:quantity state)
                      :on-change #(om/set-state! owner :quantity (.. % -target -value))}]
         [:button "Add to deck"]
         (let [query (:query state)
               matches (match (get-in state [:deck :identity]) query)]
           (when-not (or (empty? query)
                         (= (:title (first matches)) query))
             (om/set-state! owner :matches matches)
             [:div.typeahead
              (for [i (range (count matches))]
                [:div {:class (if (= i (:selected state)) "selected" "")
                       :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                         (om/set-state! owner :query (.. e -target -innerHTML))
                                         (om/set-state! owner :selected i))}
                 (:title (nth matches i))])]))]]))))

(defn deck-builder [{:keys [decks] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :edit-channel (chan)
       :deck nil})

    om/IWillMount
    (will-mount [this]
      (let [edit-channel (om/get-state owner :edit-channel)]
        (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card))))
        (go (while true
              (let [edit (<! edit-channel)
                    card (:card edit)
                    max-qty (or (:limited card) 3)
                    cards (om/get-state owner [:deck :cards])
                    match? #(when (= (get-in % [:card :title]) (:title card)) %)
                    existing-line (some match? cards)]
                (let [new-qty (+ (or (:qty existing-line) 0) (:qty edit))
                      rest (remove match? cards)
                      new-cards (cond (> new-qty max-qty) (conj rest {:qty max-qty :card card})
                                      (<= new-qty 0) rest
                                      :else (conj rest {:qty new-qty :card card}))]
                  (om/set-state! owner [:deck :cards] new-cards))
                (deck->str owner)))))
      (go (while true
            (om/set-state! owner :deck (<! select-channel)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:div.deckbuilder.blue-shade.panel
         [:div.viewport {:ref "viewport"}
          [:div.decks
           [:div.button-bar
            [:button {:on-click #(new-deck "Corp" owner)} "New Corp deck"]
            [:button {:on-click #(new-deck "Runner" owner)} "New Runner deck"]]
           [:div.deck-collection
            (if (empty? decks)
              [:h4 "You have no deck"]
              (for [deck (sort-by :date > decks)]
                [:div.deckline {:class (when (= (om/get-state owner :deck) deck) "active")
                                :on-click #(put! select-channel deck)}
                 [:img {:src (image-url (:identity deck))}]
                 (when-not (valid? deck)
                   [:div.float-right.invalid "Invalid deck"])
                 [:h4 (:name deck)]
                 [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY - HH:mm"))]
                 [:p (get-in deck [:identity :title])]]))]
           [:div {:class (when (:edit state) "edit")}
            (when-let [card (om/get-state owner :zoom)]
              (om/build card-view card))]]

          [:div.decklist
           (when-let [deck (:deck state)]
             (let [identity (:identity deck)
                   cards (:cards deck)]
               [:div
                (if (:edit state)
                  [:div.button-bar
                   [:button {:on-click #(save-deck cursor owner)} "Save"]
                   [:button {:on-click #(end-edit owner)} "Cancel"]]
                  [:div.button-bar
                   [:button {:on-click #(edit-deck owner)} "Edit"]
                   [:button {:on-click #(handle-delete cursor owner)} "Delete"]
                   [:a.button {:href (octgn-link owner) :download (str (:name deck) ".o8d")} "OCTGN Export"]])
                [:h3 (:name deck)]
                [:div.header
                 [:img {:src (image-url identity)}]
                 [:h4.fake-link {:on-mouse-enter #(put! zoom-channel identity)
                                 :on-mouse-leave #(put! zoom-channel false)} (:title identity)]
                 (let [count (card-count cards)
                       min-count (:minimumdecksize identity)]
                   [:div count " cards"
                    (when (< count min-count)
                      [:span.invalid (str "(minimum " min-count ")")])])
                 (let [inf (influence deck)
                       limit (:influencelimit identity)]
                   [:div "Influence: "
                    [:span {:class (when (> inf limit) "invalid")} inf]
                    "/" (:influencelimit identity)])
                 (when (= (:side identity) "Corp")
                   (let [min-point (min-agenda-points deck)
                         points (agenda-points deck)]
                     [:div "Agenda points: " points
                      (when (< points min-point)
                        [:span.invalid "(minimum " min-point ")"])
                      (when (> points (inc min-point))
                        [:span.invalid "(maximum" (inc min-point) ")"])]))]
                [:div.cards
                 (for [group (sort-by first (group-by #(get-in % [:card :type]) cards))]
                   [:div.group
                    [:h4 (str (or (first group) "Unknown") " (" (card-count (last group)) ")") ]
                    (for [line (sort-by #(get-in % [:card :title]) (last group))]
                      [:div.line
                       (when (:edit state)
                         (let [ch (om/get-state owner :edit-channel)]
                           [:span
                            [:button.small {:on-click #(put! ch {:qty 1 :card (:card line)})
                                            :type "button"} "+"]
                            [:button.small {:on-click #(put! ch {:qty -1 :card (:card line)})
                                            :type "button"} "-"]]))
                       (:qty line) " "
                       (if-let [name (get-in line [:card :title])]
                         (let [card (:card line)]
                           [:span
                            [:span {:class (if (allowed? card identity) "fake-link" "invalid")
                                    :on-mouse-enter #(put! zoom-channel card)
                                    :on-mouse-leave #(put! zoom-channel false)} name]
                            (when-not (or (= (:faction card) (:faction identity))
                                          (zero? (:factioncost card)))
                              (let [influence (* (:factioncost card) (:qty line))]
                                [:span.influence
                                 {:class (-> card :faction .toLowerCase (.replace " " "-"))
                                  :dangerouslySetInnerHTML
                                  #js {:__html (apply str (for [i (range influence)] "&#8226;"))}}]))])
                         (:card line))])])]]))]

          [:div.deckedit
           [:div
            [:p
             [:h3 "Deck name"]
             [:input.deckname {:type "text" :placeholder "Deck name"
                               :ref "deckname" :value (get-in state [:deck :name])
                               :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:h3 "Identity"]
             [:select.identity {:value (get-in state [:deck :identity :title])
                                :on-change #(om/set-state! owner [:deck :identity] (get-card (.. % -target -value)))}
              (for [card (side-identities (get-in state [:deck :identity :side]))]
                [:option (:title card)])]]
            (om/build card-lookup cursor {:state state})
            [:h3 "Decklist"
             [:span.small "(Type or paste a decklist, it will be parsed)" ]]]
           [:textarea {:ref "deck-edit" :value (:deck-edit state)
                       :on-change #(handle-edit owner)}]]]]]))))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})
