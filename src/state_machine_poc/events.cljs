(ns state-machine-poc.events)


(defn post->message [{:keys [title]} status]
  (str "Your post, “" title ",” has been " (name status) "."))

(defn time* []
  (let [dt (js/Date.)]
    (str (.getHours dt) ":" (.getMinutes dt) ":" (.getSeconds dt))))

(defn notify-author! [post status appstate]
  (let [author-name (get-in appstate [:users (:author post) :name])
        message (post->message post status)]
    {:at (time*)
     :notified author-name
     :message message}))

(defn notify-reviewers! [post _]
  (let [{:keys [title]} post]
    {:at (time*)
     :notified "Reviewers"
     :message (str "The post “" title ",” is ready for review")}))