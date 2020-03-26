(ns state-machine-poc.fsm)


(defmulti meets? (fn [condition _]
                   condition))

(defmethod meets? :default [condition _ caps]
  (condition caps))


(defn collect-events [{:keys [triggers]} old-status new-status]
  (reduce (partial reduce conj)
          #{}
          [(get triggers [:<- old-status])
           (get triggers [:-> new-status])
           (get triggers [old-status new-status])]))


(defn available [k state machine & args]
  (map :-> (filter (fn [{conditions :if}]
                     (every? #(apply meets? % state args) conditions))
                   ((k state) machine))))