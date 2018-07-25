(in-ns 'game.cards.identities)

(def card-definition-new-angeles-sol-your-news
  {"New Angeles Sol: Your News"
   (let [nasol {:optional
                {:prompt "Play a Current?" :player :corp
                 :req (req (not (empty? (filter #(has-subtype? % "Current")
                                                (concat (:hand corp) (:discard corp))))))
                 :yes-ability {:prompt "Select a Current to play from HQ or Archives"
                               :show-discard true
                               :async true
                               :choices {:req #(and (has-subtype? % "Current")
                                                    (= (:side %) "Corp")
                                                    (#{[:hand] [:discard]} (:zone %)))}
                               :msg (msg "play a current from " (name-zone "Corp" (:zone target)))
                               :effect (effect (play-instant eid target))}}}]
     {:events {:agenda-scored nasol :agenda-stolen nasol}})})
