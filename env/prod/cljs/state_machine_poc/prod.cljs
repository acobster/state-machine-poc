(ns state-machine-poc.prod
  (:require
    [state-machine-poc.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
