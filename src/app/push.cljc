(ns app.push
  (:import [hyperfiddle.electric Pending]
           #?(:clj [java.time Duration Instant]))
  (:require
   #?(:clj [datalevin.core :as d])
   [app.utils :as u]
   [app.tailwind :refer [tw]]
   #?(:clj [app.tx :as tx])
   #?(:clj [app.db :as db])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [hyperfiddle.rcf :refer [tests]]
   ;; #?(:clj
   ;;    [clojure.tools.macro :as macro])
   [missionary.core :as m]
   [contrib.data :refer [nil-or-empty?]]
   #?(:cljs ["dayjs" :as dayjs])
   #?(:cljs ["dayjs/plugin/utc" :as utc])
   #?(:cljs ["dayjs/plugin/timezone" :as timezone])
   #?(:cljs ["dayjs/plugin/localizedFormat" :as localizedFormat])
   #?(:cljs ["dayjs/plugin/advancedFormat" :as advancedFormat])))

#?(:cljs
   (do
     (.extend dayjs utc)
     (.extend dayjs timezone)
     (.extend dayjs localizedFormat)
     (.extend dayjs advancedFormat)))

#?(:clj
   (do
     ;; add user, creation date, root-tasks
     (defonce schema {:user/username         {:db/valueType :db.type/string
                                              :db/unique    :db.unique/identity}
                      :user/task             {:db/cardinality :db.cardinality/many
                                              :db/valueType   :db.type/ref}
                      :user/running-task     {:db/valueType :db.type/ref}
                      :user/running-interval {:db/valueType :db.type/ref}
                      :task/subtask          {:db/cardinality :db.cardinality/many
                                              :db/valueType   :db.type/ref}
                      :task/name             {}
                      :task/interval         {:db/cardinality :db.cardinality/many
                                              :db/valueType   :db.type/ref}
                      :task/history-show     {:db/valueType :db.type/boolean}
                      :task/toggled          {:db/valueType :db.type/boolean}
                      :task/time-group       {} ; :hour, :day, or :month
                      ;; for datalevin, {:db/valueType :db.type/instant}
                      ;; but not working rn
                      ;; for datascript, remove
                      :interval/start        {}
                      :interval/end          {}
                      :interval/note         {:db/valueType :db.type/string}})
     (defonce !conn
       ;; (d/create-conn schema)
       (d/get-conn "datalevin/db" schema))
     (def delay-amount 0)
     (defonce !present (atom {}))
     (comment
       (defn delete-all-intervals [conn]
         (let [intervals (d/q '[:find ?e
                                :where
                                [?e :task/interval]]
                              @conn)]
           (d/transact conn (mapv (fn [[e]] [:db/retract e :task/interval]) intervals))))
       (delete-all-intervals !conn))
     (comment
       (tx/transact! !conn [{:db/id 87 :user/task [{:db/id     -1
                                                    :task/name "Play"}
                                                   {:db/id     -2
                                                    :task/name "Metaphysics"}]}]))))


(e/def db)
(e/def present (e/server (e/watch !present)))
(e/def session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"])))
(e/def username (e/server (get-in e/*http-request* [:cookies "username" :value])))
(e/def user (d/entity (e/watch !conn) [:user/username username]))
(e/def user-id (e/server (:db/id user)))
(e/def running-task-id (e/server (->> user :user/running-task :db/id)))
(e/def running-interval (e/server (->> user :user/running-interval)))
(e/def running-interval-id (e/server (->> running-interval :db/id)))
(e/def running-start (e/server (->> running-interval :interval/start)))
(e/def running-note (e/server (->> running-interval :interval/note)))
#?(:cljs
   (def timezone "Asia/Seoul"))

#?(:clj
   (do
     ;; (def !how-many (atom 0))
     (defn get-running-history [db user-id]
       ;; (swap! !how-many inc)
       (concat
        (->>
         (d/q '[:find (pull ?i [:interval/start]) ?subtask
                :in $ % ?u
                :where
                [?u :user/task ?t]
                (or (ancestor ?t ?subtask)
                    [(= ?t ?subtask)])
                [?subtask :task/interval ?i]]
              @!conn
              '[[(ancestor ?e ?a)
                 [?e :task/subtask ?a]]
                [(ancestor ?e ?a)
                 [?e :task/subtask ?b]
                 (ancestor ?b ?a)]]
              user-id)
         (sort-by (comp :interval/start first))
         (map second))
        (when-let [running-task-id
                   (:db/id (:user/running-task (d/entity db user-id)))]
          [running-task-id])))))

(e/def running-history (e/server (get-running-history (e/watch !conn)
                                                      user-id)))

#?(:cljs
   (e/def !selected-task-id (atom (e/snapshot running-task-id))))
(e/def selected-task-id (e/client (e/watch !selected-task-id)))

(e/defn CancelRunningTask []
  (e/server
   (tx/transact! !conn
                 [[:db/retract user-id :user/running-task]
                  [:db/retract user-id :user/running-interval]])))

(e/defn StopRunningTask []
  ;; when you refactor this, refactor RunSelectedTask
  (e/server
   (let [now (System/currentTimeMillis)]
     (tx/transact! !conn
                   [{:db/id        (e/snapshot running-interval-id)
                     :interval/end now}
                    {:db/id         (e/snapshot running-task-id)
                     :task/interval (e/snapshot running-interval-id)}
                    [:db/retract user-id :user/running-task]
                    [:db/retract user-id :user/running-interval]]))))

(e/defn RunSelectedTask []
  (e/server
   (let [now (System/currentTimeMillis)]
     (tx/transact! !conn
                   (concat
                    (when (e/snapshot running-task-id)
                      [{:db/id        (e/snapshot running-interval-id)
                        :interval/end now}
                       {:db/id         (e/snapshot running-task-id)
                        :task/interval (e/snapshot running-interval-id)}])
                    [{:db/id                 user-id
                      :user/running-task     (e/snapshot selected-task-id)
                      :user/running-interval {:db/id          -1
                                              :interval/start now
                                              :interval/note  ""}}])))))

(e/defn Toggle [!toggled]
  (let [toggled (e/watch !toggled)]
    (ui/button
      (e/fn []
        (swap! !toggled not))
      (dom/props {:class (tw "ml-1 btn-[* xs] w-fit px-1")})
      (dom/on "click" (e/fn [e]
                        (.stopPropagation e)))
      (svg/svg (dom/props {:width  10
                           :height 10})
               (when toggled
                 ;; vertical line
                 (svg/line (dom/props {:x1     5,       :x2           5
                                       :y1     0.2,     :y2           9.8
                                       :stroke "black", :stroke-width 1})))
               ;; horizontal line
               (svg/line (dom/props {:x1     0,       :x2           10
                                     :y1     5,       :y2           5
                                     :stroke "black", :stroke-width 1}))))))

(e/defn TaskActionButtons [task-id]
  (dom/div
    (dom/props {:class "ml-1 flex"})
    (ui/button
      (e/fn []
        (e/server
         (tx/transact! !conn [{:db/id             task-id
                               :task/subtask      [{:db/id     -1
                                                    :task/name "~stub~"}]
                               :task/toggled      false
                               :task/history-show true}])))
      (dom/props {:class (tw "btn-[* xs]")})
      (dom/on "click" (e/fn [e] (.stopPropagation e)))
      (dom/text "Add"))
    (let [!edit-text (atom nil), edit-text (e/watch !edit-text)]
      (dom/div
        (dom/props {:class "flex"})
        (ui/button
          (e/fn []
            (if @!edit-text
              (do
                (e/server (tx/transact! !conn [{:db/id task-id :task/name edit-text}]))
                (reset! !edit-text nil))
              (reset! !edit-text (e/server (:task/name (d/entity db task-id))))))
          (dom/props {:class (tw "btn-[* xs]")})
          (dom/on "click" (e/fn [e] (.stopPropagation e)))
          (dom/text
           (if edit-text
             "Confirm"
             "Edit")))
        (when edit-text
          (ui/input
           edit-text
           (e/fn [v]
             (reset! !edit-text v))
           (dom/props {:class (tw
                               "input-[* sm] absolute border-[black 2] mt-6 w-32")})
           (dom/on "click"
                   (e/fn [e]
                     (.stopPropagation e)))
           (dom/on "keydown"
                   (e/fn [e]
                     (.stopPropagation e)
                     (when (u/in? ["Enter" "Escape"] (.-key e))
                       (.preventDefault e)
                       (if (= (.-value (.-target e)) "")
                         ;; deletion
                         (when (and (not (e/server (:task/subtask (d/entity db task-id))))
                                    (not (= task-id running-task-id)))
                           (e/server
                            (tx/transact! !conn [[:db.fn/retractEntity task-id]]))
                           (when (= task-id selected-task-id)
                             (reset! !selected-task-id nil)))
                         ;; edit
                         (do
                           (e/server (tx/transact!
                                      !conn [{:db/id task-id :task/name edit-text}]))
                           (reset! !edit-text nil))))))))))))

(e/def TaskList)
(e/defn TasksPanel []
  (dom/div
    (dom/props {:class "grow sm:min-w-[250px] sm:max-w-[250px] min-h-[14rem] sm:min-h-[18rem] min-h-full sm:h-none mb-2 sm:mb-0 p-2 rounded bg-base-200"})
    ;; (dom/on "click"
    ;;         (e/fn [e]
    ;;           (reset! !selected-task-id nil)))
    (let [!editing           (atom false), editing           (e/watch !editing)
          !task-list-toggled (atom false), task-list-toggled (e/watch !task-list-toggled)]
      (dom/div
        (dom/props {:class "text-xl font-bold flex items-center"})
        (ui/button
          (e/fn [] (reset! !selected-task-id running-task-id))
          (dom/on "click" (e/fn [e]
                            (.stopPropagation e)))
          (dom/text "Day 5"))
        (Toggle. !task-list-toggled)
        (ui/button
          (e/fn [] (swap! !editing not))
          (dom/props {:class (tw "ml-2 btn-[* xs] mt-[1px] bg-base-100")})
          (dom/on "click" (e/fn [e]
                            (.stopPropagation e)))
          (dom/text (if editing "stop editing" "edit"))))
      (dom/div
        (dom/props {:class (when task-list-toggled
                             "hidden")})
        (binding
            [TaskList
             (e/fn [task-ids]
               (e/for [[is-last task-id]
                       (map-indexed (fn [idx task-id]
                                      [(= idx (dec (count task-ids))) task-id])
                                    task-ids)]
                 (let [task        (e/server
                                    (:task/name (d/entity db task-id)))
                       subtask-ids (e/server
                                    (sort (map :db/id (:task/subtask (d/entity db task-id)))))
                       !toggled    (u/e-relay-atom task-id :task/toggled)]
                   (dom/div
                     (dom/props
                      {:class (tw "flex justify-between"
                                  (when (= task-id running-task-id)
                                    "font-bold"))})
                     (ui/button
                       (e/fn []
                         ;; for disabling double-click
                         (if (= selected-task-id task-id)
                           (when-not (= selected-task-id running-task-id)
                             (e/server
                              (RunSelectedTask.)))
                           (reset! !selected-task-id task-id)))
                       (dom/props {:class "text-left flex"})
                       (dom/on "click"
                               (e/fn [e]
                                 (.stopPropagation e)))
                       (dom/div
                         (dom/props {:class (when (= task-id selected-task-id)
                                              "underline")})
                         (dom/text task))
                       (dom/div
                         (dom/props {:class (when-not (and (seq subtask-ids)
                                                           (not editing))
                                              "hidden")})
                         (Toggle. !toggled)))
                     (dom/div
                       (dom/props {:class (when-not editing
                                            "hidden")})
                       (TaskActionButtons. task-id)))
                   (dom/div
                     (dom/props {:class "ml-2"})
                     (dom/div
                       (dom/props {:class
                                   (when (e/watch !toggled)
                                     "hidden")})
                       (TaskList. subtask-ids))))))]
          (dom/props {:class "mt-[2px]"})
          (TaskList. (e/server
                      (map d/datom-v
                           (d/datoms db :eav user-id :user/task)))))))))

(e/defn SelectTaskButton [target-id props]
  (ui/button
    (e/fn []
      (reset! !selected-task-id target-id)
      (doseq [ancestor-task-id (e/server (db/get-ancestor-task-ids db target-id))]
        (e/server
         (tx/transact! !conn [{:db/id        ancestor-task-id
                               :task/toggled false}]))))
    (dom/props props)
    (dom/text
     (e/server (:task/name (d/entity db target-id))))))

(e/defn Breadcrumbs []
  (let [cutoff       4
        max-cutoff   10
        !show-cutoff (atom false)
        show-cutoff  (e/watch !show-cutoff)]
    (dom/div
      (dom/props {:class (tw "flex text-xs"
                             "ml-[-6px] mt-[-3px]")})
      (ui/button
        (e/fn []
          (swap! !show-cutoff not))
        (dom/props {:class "w-3 font-bold mt-[-6.5px] h-6"})
        (dom/text "."))
      (dom/div
        (dom/props {:class "ml-[1px] mt-[-10px] mb-[-8px] text-xs breadcrumbs"})
        (dom/ul
          (let [ ;; running-history (e/server (e/watch !running-history))
                ordered-ancestor-ids
                (e/server (vec (map #(:db/id (d/entity db %))
                                    (db/get-ancestor-task-ids db selected-task-id))))
                ;; breadcrumbs-task-ids
                bc-task-ids (take-last (if show-cutoff
                                         max-cutoff cutoff)
                                       ;; remove sequential dups
                                       (->> running-history
                                            (remove nil?)
                                            (partition-by identity)
                                            (map first)))]
            (e/for [[is-last bc-task-id]
                    (map-indexed (fn [idx bc-task-id]
                                   [(= idx (dec (count bc-task-ids))) bc-task-id])
                                 bc-task-ids)]
              (dom/li
                (dom/div
                  (dom/props {:class "mt-[1.5px]"})
                  (SelectTaskButton. bc-task-id
                                     {:class
                                      (str
                                       "hover:underline"
                                       (when (if (= selected-task-id running-task-id)
                                               is-last
                                               (= selected-task-id bc-task-id))
                                         " underline"))}))))))))))

(e/defn SelectedStatus []
  (let [ancestor-task-ids   (e/server (db/get-ancestor-task-ids db running-task-id))
        descendant-task-ids (e/server (db/get-descendant-task-ids db running-task-id))]
    (dom/div
      (dom/props {:class "text-sm"})
      (cond (u/in? ancestor-task-ids selected-task-id)
            (dom/div
              (dom/props {:class "mt-[2px]"})
              (dom/text "Ancestor of currently running ")
              (SelectTaskButton. running-task-id
                                 {:class "text-xs italic"}))
            (and
             running-task-id           ; we should have something running
             (= selected-task-id running-task-id))
            (let [duration (e/server
                            (int
                             (/ (- e/system-time-ms running-start)
                                1000)))]
              (dom/div
                (dom/props {:class "mt-[2px]"})
                (if (= duration 0)
                  (dom/text "Starting "
                            (e/server
                             (-> (d/entity db selected-task-id)
                                 :task/name)))
                  (dom/text "Elapsed for " duration " s"))))
            (u/in? descendant-task-ids selected-task-id)
            (dom/div
              (dom/props {:class "mt-[2px]"})
              (dom/text "Descendant of currently running ")
              (SelectTaskButton. running-task-id
                                 {:class "text-xs italic"}))))))

(e/defn RunButtons []
  (let [is-running (= running-task-id selected-task-id)]
    (dom/div
      (dom/props {:class "flex gap-2"})
      (ui/button
        (e/fn []
          (e/server
           (if is-running
             (StopRunningTask.)
             (RunSelectedTask.))))
        (dom/props {:class (tw "btn-[* xs] block bg-base-300"
                               "animation-none hover:bg-base-100")})
        (dom/text
         (if is-running
           "Stop"
           "Start")))
      (when is-running
        (ui/button
          (e/fn [] (CancelRunningTask.))
          (dom/props {:class (tw "btn-[* xs] block bg-base-300"
                                 "animation-none hover:bg-base-100")})
          (dom/text "Cancel"))))))

(defn tz-dayjs
  ([x]
   #?(:cljs
      (.tz
       (new dayjs x)
       "Asia/Seoul")))
  ([]
   #?(:cljs
      (.tz
       (new dayjs)
       "Asia/Seoul"))))

(defn new-dayjs
  ([x]
   #?(:cljs
      (new dayjs x)))
  ([]
   #?(:cljs
      (new dayjs))))

#?(:cljs
   (defn map-vals [f m]
     (into {} (for [[k v] m] [k (f v)]))))

(e/defn SelectedPanel []
  (dom/div
    (dom/props {:class "grow pl-2 py-2 pr-4 sm:ml-2 rounded bg-base-200 sm:min-h-full"})
    (dom/div
      (Breadcrumbs.))
    (if selected-task-id
      (dom/div
        (dom/div
          (dom/props {:class "text-lg"})
          ;; e.g. Learn Libraries
          (dom/text (e/server
                     (-> (d/entity db selected-task-id)
                         :task/name))) )
        (dom/div
          (dom/props {:class "ml-2 mt-[1px] h-[700px] flex flex-col"})
          ;; start/stop/cancel
          (dom/div
            (dom/props {:class "mb-[5px]"})
            (RunButtons.))
          ;; e.g. elapsed for...
          (SelectedStatus.)
          ;; e.g. 8/7 4:35 ~ 8/7 4:36
          ;; (letfn [(to-date [ms]
          ;;           (let [date    (js/Date. (+ ms (* 9 60 60 1000))) ; KST is UTC+9
          ;;                 month   (.getUTCMonth date)
          ;;                 day     (.getUTCDate date)
          ;;                 hours   (.getUTCHours date)
          ;;                 minutes (.getUTCMinutes date)]
          ;;             (str (inc month) "/" day " " hours ":" minutes)))]
          ;;   (dom/div
          ;;     (e/for [[start end] (e/server
          ;;                          (reverse
          ;;                           (map #(vector (:interval/start %)
          ;;                                         (:interval/end %))
          ;;                                (sort-by
          ;;                                 :interval/start
          ;;                                 (:task/interval (d/entity db selected-task-id))))))]
          ;;       (dom/div
          ;;         (dom/text
          ;;          (to-date start)
          ;;          " ~ "
          ;;          (to-date end))))))
          (dom/div
            (dom/props {:class (tw "flex-[* col] gap-3 mt-1 mb-2")})
            (let [past-notes    (e/server
                                 (->> (d/entity db selected-task-id)
                                      :task/interval
                                      (sort-by :interval/start)
                                      (remove #(nil-or-empty? (:interval/note %)))
                                      (map #(select-keys % [:db/id :interval/start
                                                            :interval/note]))))
                  !history-show (u/selected-task-relay-atom :task/history-show)
                  history-show  (e/watch !history-show)
                  !time-group   (u/selected-task-relay-atom :task/time-group)
                  time-group    (e/watch !time-group)
                  !show-full    (u/selected-task-relay-atom :task/show-full)
                  show-full     (e/watch !show-full)]

              (when-not (nil-or-empty? past-notes)
                (dom/div
                  (dom/props {:class (tw "flex gap-2")})
                  (ui/button
                    (e/fn [] (swap! !history-show not))
                    (dom/props {:class (tw "btn-[* xs] bg-base-300 hover:bg-base-100")})
                    (dom/text
                     (if history-show
                       "hide past notes"
                       "show past notes")))
                  (when history-show
                    (ui/button
                      (e/fn []
                        (swap! !time-group (fn [tg]
                                             (cond (= tg :hour)
                                                   :day
                                                   (= tg :day)
                                                   :month
                                                   (= tg :month)
                                                   :hour
                                                   :else :day))))
                      (dom/props {:class (tw "btn-[* xs] bg-base-300 hover:bg-base-100")})
                      (dom/text (str "group by "
                                     (name
                                      (case time-group
                                        :hour  :day
                                        :day   :month
                                        :month :hour)))))
                    (when (= running-task-id selected-task-id)
                      (ui/button
                        (e/fn []
                          (swap! !show-full not))
                        (dom/props {:class (tw "btn-[* xs] bg-base-300 hover:bg-base-100")})
                        (dom/text
                         (if show-full
                           "show short"
                           "show full")))))))
              (when (not (nil-or-empty? past-notes))
                (dom/div
                  (dom/props {:class (tw "flex-[* col] gap-3 mb-2"
                                         (when (and (not show-full)
                                                    (= selected-task-id
                                                       running-task-id))
                                           "max-h-96 overflow-auto")
                                         (when-not history-show
                                           "hidden"))})
                  (e/for [[range-time same-range-note-maps]
                          (group-by
                           #(.format
                             (tz-dayjs (:interval/start %))
                             ;; L is MM/DD/YYYY
                             ;; these formats do the following:
                             ;; 1) checking if same hour/day/month
                             ;; 2) possibility of reverting on .dayjs call
                             (cond (= time-group :hour)  "L HH:00"
                                   (= time-group :day)   "L"
                                   (= time-group :month) "MMMM YYYY"
                                   :else                 "L"))
                           past-notes)]
                    (when-not (nil-or-empty? same-range-note-maps)
                      (dom/div
                        (dom/text
                         (cond (and (u/in? [:hour :day] time-group)
                                    (= (.format (tz-dayjs e/system-time-ms) "L")
                                       (.format (new-dayjs range-time) "L")))
                               (str "Today"
                                    (when (= time-group :hour)
                                      (str " at "
                                           (.format (new-dayjs range-time) " h A"))))
                               (and (u/in? [:hour :day] time-group)
                                    (= (.format (.subtract
                                                 (tz-dayjs e/system-time-ms)
                                                 1 "day") "L")
                                       (.format (new-dayjs range-time) "L")))
                               (str "Yesterday"
                                    (when (= time-group :hour)
                                      (str " at "
                                           (.format (new-dayjs range-time) " h A"))))
                               :else
                               (.format (new-dayjs range-time)
                                        (case time-group
                                          :hour  "MMMM Do h A"
                                          :day   "MMMM Do"
                                          :month "MMMM YYYY"
                                          "MMMM Do")))))
                      (dom/div
                        (dom/props {:class (tw "bg-base-100 card py-[4px] px-3")})
                        (e/for [note (map :interval/note same-range-note-maps)]
                          (dom/div
                            ;; (dom/props {:class
                            ;;             (tw "border-[b gray-400] last:border-b-0")})
                            (dom/div
                              (dom/props {:class (tw "card-body p-[2px]"
                                                     "whitespace-pre-line")})
                              (dom/text note)))))))))))
          (u/textarea*
           running-note
           (e/fn [v]
             (e/server
              (tx/transact! !conn [{:db/id         running-interval-id
                                    :interval/note v}])))
           (dom/style {:background-color "bg-base-200"})
           (dom/props {:class (tw "textarea-[* sm primary]"
                                  "w-full text-md"
                                  "grow focus:outline-none"
                                  (when-not (= selected-task-id running-task-id)
                                    "invisible"))}))))
      (dom/text "Select any task."))))

(e/def some-value (e/server (:value (d/entity (e/watch !conn) 1000))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (if-not (some? username)
        (dom/div
          (dom/text "Set login cookie here: ")
          (dom/a (dom/props {::dom/href "/auth"
                             :class     "text-green-500"})
                 (dom/text "/auth"))
          (dom/text " (blank password)"))
        (do
          (when-not user-id
            (e/server
             (tx/transact! !conn [{:db/id -1 :user/username username}])))
          (e/server
           (swap! !present assoc session-id username)
           (e/on-unmount #(swap! !present dissoc session-id)))
          ;; (dom/div (dom/text "[" user-id "] " username))
          (when user-id
            (dom/div
              (dom/props {:class "m-10 sm:flex h-fit min-w-[14rem]"})
              (dom/style {:touch-action "manipulation"})
              (TasksPanel.)
              (SelectedPanel.)))))))))
