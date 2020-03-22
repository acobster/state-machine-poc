(ns state-machine-poc.core
    (:require
     [state-machine-poc.fsm :as fsm]
     [clojure.string :refer [split join]]
     [reagent.core :as r]
     [reagent.dom :as d]))


(defn anarchy [statuses]
  (letfn [(->transition [status] {:-> status})
          (->status-transitions [status]
                                {status (vec
                                         (map ->transition (disj statuses status)))})]
    (into {} (map ->status-transitions statuses))))

(defonce templates {:anarchy (anarchy #{:draft :in-review :approved :published :deleted})
                    :standard {:draft [{:-> :in-review}
                                       {:-> :published :if [:can-publish?]}
                                       {:-> :deleted :if [:can-delete?]}]
                               :in-review [{:-> :approved :if [:can-approve?]}
                                           {:-> :published :if [:can-publish?]}
                                           {:-> :deleted :if [:can-delete?]}]
                               :approved [{:-> :published :if [:can-publish-approved?]}
                                          {:-> :deleted :if [:can-delete?]}]
                               :published [{:-> :draft :if [:can-unpublish?]}
                                           {:-> :in-review :if [:can-unpublish?]}
                                           {:-> :deleted :if [:can-delete?]}]
                               :deleted [{:-> :draft}]}})

(defonce template-descriptions {:anarchy "Anyone can do anything, even if they don't have “permission.”"
                                :standard "This is a sensible default."})


(defonce appstate (r/atom {:flow (:standard templates)
                           :template :standard
                           :posts [{:status :draft
                                    :title "Example Post Title"}
                                   {:status :published
                                    :title "An Older Post"}
                                   {:status :deleted}]
                           :currently-editing 0
                           :users [{:name "Alice"
                                    :capabilities {:can-approve? true
                                                   :can-delete? true
                                                   :can-publish? true
                                                   :can-publish-approved? true
                                                   :can-unpublish? true}}
                                   {:name "Bob"
                                    :capabilities {:can-approve? false
                                                   :can-delete? false
                                                   :can-publish? false
                                                   :can-publish-approved? true
                                                   :can-unpublish? true}}]
                           :templates templates}))




(def flow (r/cursor appstate [:flow]))
(def template (r/cursor appstate [:template]))
(def post-id (r/cursor appstate [:currently-editing]))
(def capabilities (r/cursor appstate [:users 0 :capabilities]))

(defn post [] (get-in @appstate [:posts @post-id]))

(defn toggle-cap! [user-id cap]
  (swap! appstate update-in [:users user-id :capabilities cap] not))

(defn select-template! [tpl-name]
  (swap! appstate (fn [state]
                    (-> state
                        (assoc :template tpl-name)
                        (assoc :flow (tpl-name templates))))))

(defn transition! [status]
  (swap! appstate assoc-in [:posts @post-id :status] status))

(defn readable [kw]
  (join " " (split (name kw) #"-")))


(defn transition-button [to-status]
  [:button {:on-click #(transition! to-status)} (name to-status)])

(defn user [id u]
  [:div.user
   [:h3 "User: " (:name u)]
   [:div
    [:h4 "Permissions:"]
    [:div.permission-simulation
     (map (fn [[cap can?]]
            (let [id-attr (str (name cap) "-" id)]
              ^{:key cap}
              [:div
               [:input {:type "checkbox"
                        :id id-attr
                        :checked can?
                        :on-change #(toggle-cap! id cap)}]
               [:label {:for id-attr} (readable cap)]]))
          (:capabilities u))]]
   [:h4 "With the above permissions, " (:name u) " would see:"]
   [:div.post-simulation
    [:h4.post-title (get-in @appstate [:posts @post-id :title])]
    [:div.post-status "Status: " (get-in @appstate [:posts @post-id :status])]
    [:div.post-actions
     (map (fn [to-status]
            ^{:key to-status}
            [transition-button to-status])
          (fsm/available :status (post) @flow (:capabilities u)))]]])

(defn template-dropdown []
  [:select {:default-value @template}
   (map (fn [[tpl _]]
          ^{:key tpl}
          [:option {:on-click #(select-template! tpl)} (name tpl)])
        templates)])


(defn status-transition [j trans]
  (let [{to :-> conditions :if} trans]
    [:li.status-transition
     [:span.status-name (name to)]
     (if (seq conditions)
       [:span.conditions
        [:i " if "]
        (map (fn [cnd]
               [:span.cond-name (readable cnd)])
             conditions)]
       [:i " unconditionally"])]))

(defn workflow-vis []
  [:div.workflow-vis
   [:div.box
    [:h4 "Start with the " [template-dropdown] " workflow..."]
    [:div.instruct "You are using the " (name @template) " template. " (@template template-descriptions)]]
   [:h3 "A user can transition a post..."]
   [:ul.workflow-statuses
    (map-indexed (fn [i [status transitions]]
                   ^{:key i}
                   [:li.workflow-status
                    [:h5 "From " [:span.status-name (name status)] " status ..."]
                    [:ul.status-transitions
                     (map-indexed (fn [j trans]
                                    ^{:key j}
                                    [status-transition j trans])
                                  transitions)]])
                 @flow)]])


(defn home-page []
  [:div [:h1 "Publishing workflow editor proof-of-concept"]
   [:main
    [workflow-vis]
    [:div.box
     [:h4 "Simulator"]
     [:div.instruct "Use this to simulate how users can transition posts, given certain permissions."]
     [:div.users
      (map-indexed (fn [i u]
                     ^{:key i}
                     [user i u])
                   (:users @appstate))]]]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
