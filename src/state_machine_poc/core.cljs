(ns state-machine-poc.core
    (:require
     [state-machine-poc.fsm :as fsm]
     [clojure.string :refer [split join]]
     [clojure.set :refer [difference]]
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
                                       {:-> :published :if #{:can-publish?}}
                                       {:-> :deleted :if #{:can-delete?}}]
                               :in-review [{:-> :approved :if #{:can-approve?}}
                                           {:-> :published :if #{:can-publish?}}
                                           {:-> :deleted :if #{:can-delete?}}]
                               :approved [{:-> :published :if #{:can-publish-approved?}}
                                          {:-> :deleted :if #{:can-delete?}}]
                               :published [{:-> :draft :if #{:can-unpublish?}}
                                           {:-> :in-review :if #{:can-unpublish?}}
                                           {:-> :deleted :if #{:can-delete?}}]
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
                           :conditions #{:can-approve? :can-delete? :can-publish-approved? :can-publish? :can-unpublish? :can-draft?}
                           :templates templates}))




(def flow (r/cursor appstate [:flow]))
(def template (r/cursor appstate [:template]))
(def post-id (r/cursor appstate [:currently-editing]))
(def capabilities (r/cursor appstate [:users 0 :capabilities]))
(def conditions (r/cursor appstate [:conditions]))

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

(defn add-transition! [from to]
  (swap! appstate update-in [:flow from] conj {:-> to}))

(defn delete-transition! [status idx]
  (swap! appstate update-in [:flow status] (fn [transitions]
                                             (let [[before rest] (split-at idx transitions)]
                                               (concat before (next rest))))))

(defn add-condition! [status-idx trans-idx condition]
  (let [conj-to-set (comp set conj)]
    (swap! appstate update-in [:flow status-idx trans-idx :if] conj-to-set condition)))

(defn delete-condition! [status-idx trans-idx condition]
  (swap! appstate update-in [:flow status-idx trans-idx :if] disj condition))

(defn readable [kw]
  (join " " (split (name kw) #"-")))

(defn transition-options [status]
  (let [all-statuses (set (keys @flow))
        ineligible (conj (set (map :-> (status @flow))) status)]
    (difference all-statuses ineligible)))

(defn condition-options [current-conditions]
  (difference @conditions current-conditions))

(defn imperative [status]
  (status {:draft "Save as Draft"
           :in-review "Send to Review"
           :approved "Approve"
           :published "Publish"
           :deleted "Delete"}))


(defn transition-button [to-status]
  [:button {:on-click #(transition! to-status)} (imperative to-status)])

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


(defn status-transition [trans]
  (let [{to :->
         transition-conditions :if
         on-click-add-cond :on-click-add-cond
         on-click-delete-cond :on-click-delete-cond
         on-click-delete #(js/console.log %)} trans]
    [:li.status-transition
     [:span.status-name (name to)]
     [:span.delete-transition.remove {:title "Remove this transition"
                                      :on-click on-click-delete} "×"]
     (when (seq transition-conditions)
       [:span.conditions
        [:i " if "]
        (map (fn [cnd]
               ^{:key (gensym)}
               [:span.condition
                [:span.cond-name (readable cnd)]
                [:span.remove {:on-click #(on-click-delete-cond cnd)
                               :title "Remove this condition"}
                 "×"]])
             transition-conditions)])
     [:select.add-condition {:on-change #(on-click-add-cond (keyword (.. % -target -value)))}
      [:option {:value ""} "Add a condition..."]
      (map (fn [cnd]
             ^{:key cnd}
             [:option {:value cnd}
              (name cnd)])
           (condition-options transition-conditions))]]))


(defn add-transition-select [status]
  (let [opts (transition-options status)]
    (when (seq opts)
      [:li
       [:select.add-transition {:value ""
                                :on-change #(add-transition! status (keyword (.. % -target -value)))}
        [:option {:value ""} "Add a transition..."]
        (map (fn [to-status]
               ^{:key to-status}
               [:option {:value to-status}
                (name to-status)])
             opts)]])))


(defn workflow-vis []
  [:div.workflow-vis
   [:div.box
    [:h4 "Start with the " [template-dropdown] " workflow..."]
    [:div.instruct "You are using the " (name @template) " template. " (@template template-descriptions)]]
   [:h3 "A user can transition a post..."]
   [:ul.workflow-statuses
    (map (fn [[status transitions]]
           ^{:key status}
           [:li.workflow-status
            [:span [:i "from "] [:span.status-name (name status)]]
            [:ul.status-transitions
             (map-indexed
              (fn [j trans]
                ^{:key j}
                [status-transition
                 (merge trans
                        {:on-click-add-cond (partial add-condition! status j)
                         :on-click-delete-cond (partial delete-condition! status j)
                         :on-click-delete #(delete-transition! status j)})])
              transitions)
             [add-transition-select status]]])
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
