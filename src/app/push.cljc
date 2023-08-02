(ns app.push
  (:import [hyperfiddle.electric Pending]
           #?(:clj [java.time Duration Instant]))
  (:require
   ;; when you switch this, make sure to switch tx.clj and db.clj, too
   #?(:clj [datalevin.core :as d])
   ;; #?(:clj [datascript.core :as d])
   [app.utils :as u]
   #?(:clj [app.tx :as tx])
   #?(:clj [app.db :as db])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [hyperfiddle.rcf :refer [tests]]
   [contrib.str :refer [empty->nil]]
   ;; #?(:cljs d3)
   ))

#?(:clj
   (do
     ;; add user, creation date, root-tasks
     (defonce schema {:task/subtask   {:db/cardinality :db.cardinality/many
                                       :db/valueType   :db.type/ref}
                      :task/name      {}
                      :task/interval  {:db/cardinality :db.cardinality/many
                                       :db/valueType   :db.type/ref}
                      :task/toggled   {}
                      ;; for datalevin, {:db/valueType :db.type/instant}
                      ;; but not working rn
                      ;; for datascript, remove
                      :interval/start {}
                      :interval/end   {}
                      })
     (defonce !conn
       ;; (d/create-conn schema)
       (d/get-conn "datalevin/db" schema)
       )
     (defonce !running-id (atom nil))
     (defonce !running-start (atom nil))
     (defonce !selected-id (atom nil))
     (defonce !running-history (u/make-history-atom !running-id))))
(e/def db)
(e/def running-id (e/server (e/watch !running-id)))
(e/def running-start (e/server (e/watch !running-start)))
(e/def selected-id (e/server (e/watch !selected-id)))
#?(:cljs (defonce !temp-show-task-ids (atom nil)))
(e/def temp-show-task-ids (e/client (e/watch !temp-show-task-ids)))

#?(:clj
   (do
     (defn stop-running-task []
       (let [now (System/currentTimeMillis)]
         (d/transact! !conn
                      [{:db/id         @!running-id
                        :task/interval {:db/id          -1
                                        :interval/start @!running-start
                                        :interval/end   now}}])
         (reset! !running-id nil)
         (reset! !running-start nil)))

     (defn run-selected-task []
       ;; we can't use stop-running-task because we have to sync the start and end times
       ;; much like compare-and-swap
       (let [now (System/currentTimeMillis)]
         (when @!running-id
           (d/transact! !conn
                        [{:db/id         @!running-id
                          :task/interval {:db/id          -1
                                          :interval/start @!running-start
                                          :interval/end   now}}]))
         (reset! !running-id @!selected-id)
         (reset! !running-start now)))

     (comment
       (:task/interval
        (d/entity @!conn 26))

       (defn delete-all-intervals [conn]
         (let [intervals (d/q '[:find ?e
                                :where
                                [?e :task/interval]]
                              @conn)]
           (d/transact conn (mapv (fn [[e]] [:db/retract e :task/interval]) intervals))))
       (delete-all-intervals !conn)
       (def !running-history (u/make-history-atom !running-id))
       ;; (tests
       ;;  (d/get-ancestor-task-ids @!conn 5) := [1 2]
       ;;  (set (d/get-descendant-task-ids @!conn 2)) := (set [4 5 7 8]))
       )))

(e/defn SVGHorizontalLine []
  (svg/line (dom/props {:x1     0,       :x2           10
                        :y1     5,       :y2           5
                        :stroke "black", :stroke-width 1})))
(e/defn SVGVerticalLine []
  (svg/line (dom/props {:x1     5,       :x2           5
                        :y1     0.2,     :y2           9.8
                        :stroke "black", :stroke-width 1})))

(e/defn Toggle [task-id]
  (let [toggled (if (u/in? temp-show-task-ids task-id)
                  false
                  (e/server (:task/toggled (d/entity db task-id))))]
    (ui/button
      (e/fn []
        (e/server
         (tx/transact! !conn [{:db/id task-id
                               :task/toggled
                               (not (if (some #(= % task-id) (e/client
                                                              @!temp-show-task-ids))
                                      false
                                      (:task/toggled (d/entity @!conn task-id))))}]))
        (reset! !temp-show-task-ids nil))
      (dom/props {:class (u/tw "ml-1 btn-[* xs] w-fit px-1")})
      (dom/on "click" (e/fn [e]
                        (.stopPropagation e)))
      (svg/svg (dom/props {:width  10
                           :height 10})
               (when toggled
                 (SVGVerticalLine.))
               (SVGHorizontalLine.)))))

(e/defn TaskActionButton [action text]
  (ui/button
    action
    (dom/props {:class (u/tw "btn-[* xs]")})
    (dom/on "click" (e/fn [e]
                      (.stopPropagation e)))
    (dom/text text)))

(e/defn TaskActionButtons [task-id]
  (dom/div
    (dom/props {:class "ml-1 flex"})
    (TaskActionButton.
     (e/fn []
       (e/server
        (tx/transact! !conn [{:db/id        task-id
                              :task/subtask [{:db/id     -1
                                              :task/name "~stub~"}]
                              :task/toggled false}])))
     "Add")
    (TaskActionButton.
     (e/fn []
       (when (and (not (e/server (:task/subtask (d/entity db task-id))))
                  (not (= task-id running-id)))
         (e/server
          (tx/transact! !conn [[:db.fn/retractEntity task-id]]))
         (when (= task-id selected-id)
           (e/server (reset! !selected-id nil)))))
     "Del")
    (let [!edit-text (atom nil),
          edit-text  (e/watch !edit-text)]
      (dom/div
        (dom/props {:class "flex"})
        (TaskActionButton.
         (e/fn []
           (if @!edit-text
             (do
               (e/server (tx/transact! !conn [{:db/id task-id :task/name edit-text}]))
               (reset! !edit-text nil))
             (reset! !edit-text (e/server (:task/name (d/entity db task-id))))))
         (if edit-text
           "Confirm"
           "Edit"))
        (when edit-text
          (ui/input
           edit-text
           (e/fn [v]
             (reset! !edit-text v))
           (dom/props {:class (u/tw
                               "input-[* sm] absolute border-[black 2] mt-6 w-32")})
           (dom/on "click"
                   (e/fn [e]
                     (.stopPropagation e)))
           (dom/on "keydown"
                   (e/fn [e]
                     (.stopPropagation e)
                     (when (u/in? ["Enter" "Escape"] (.-key e))
                       (.preventDefault e)
                       (e/server (tx/transact!
                                  !conn [{:db/id task-id :task/name edit-text}]))
                       (reset! !edit-text nil))))))))))

(e/def TaskList)
(e/defn TasksPanel []
  (dom/div
    (dom/props {:class "grow sm:max-w-xs min-h-[14rem] sm:min-h-[18rem] min-h-full sm:h-none mb-2 sm:mb-0 p-2 rounded bg-base-200"})
    (dom/on "click"
            (e/fn [e]
              (e/server (reset! !selected-id nil))))
    (let [!editing (atom false), editing (e/watch !editing)]
      (dom/div
        (dom/props {:class "text-xl font-bold flex items-center"})
        (ui/button
          (e/fn [] (e/server (reset! !selected-id running-id)))
          (dom/on "click" (e/fn [e]
                            (.stopPropagation e)))
          (dom/text "Day 5"))
        (ui/button
          (e/fn [] (swap! !editing not))
          (dom/props {:class (u/tw "ml-2 btn-[* xs] mt-[1px] bg-base-100")})
          (dom/on "click" (e/fn [e]
                            (.stopPropagation e)))
          (dom/text (if editing "stop editing" "edit"))))
      (dom/div
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
                                    (sort (map :db/id (:task/subtask (d/entity db task-id)))))]
                   (dom/div
                     (dom/props
                      {:class (u/tw "flex"
                                    ;; don't use cond
                                    (when (= task-id running-id)
                                      "font-bold"))})
                     (ui/button
                       (e/fn []
                         ;; for disabling double-click
                         ;; (e/server (reset! !selected-id task-id))
                         (if (= selected-id task-id)
                           (when-not (= selected-id running-id)
                             (e/server (run-selected-task)))
                           (e/server (reset! !selected-id task-id))))
                       (dom/props {:class "text-left flex"})
                       (dom/on "click"
                               (e/fn [e]
                                 (.stopPropagation e)))
                       (dom/div
                         (dom/props {:class (when (= task-id selected-id)
                                              "underline")})
                         (dom/text task))
                       (dom/div
                         (dom/props {:class (when-not (and (seq subtask-ids)
                                                           (not editing))
                                              "hidden")})
                         (Toggle. task-id)))
                     (dom/div
                       (dom/props {:class (when-not editing
                                            "hidden")})
                       (TaskActionButtons. task-id)))
                   (dom/div
                     (dom/props {:class "ml-2"})
                     (dom/div
                       (dom/props {:class
                                   (when (if (u/in? temp-show-task-ids
                                                    task-id)
                                           false
                                           (e/server
                                            (:task/toggled
                                             (d/entity db task-id))))
                                     "hidden")})
                       (TaskList. subtask-ids))))))]
          (dom/props {:class "mt-[2px]"})
          (TaskList. (e/server (db/get-root-task-ids db))))))))

(e/defn SelectTaskButton [target-id props]
  (ui/button
    (e/fn []
      (e/server
       (reset! !selected-id target-id))
      (doseq [ancestor-task-id (e/server (db/get-ancestor-task-ids db target-id))]
        (e/server
         (tx/transact! !conn [{:db/id        ancestor-task-id
                               :task/toggled false}])))
      ;; (reset! !temp-show-task-ids (e/server (db/get-ancestor-task-ids db target-id)))
      )
    (dom/props props)
    (dom/text
     (e/server (:task/name (d/entity db target-id))))))

(e/defn Breadcrumbs []
  (let [cutoff       4
        max-cutoff   10
        !show-cutoff (atom false)
        show-cutoff  (e/watch !show-cutoff)]
    (dom/div
      (dom/props {:class (u/tw "flex text-xs"
                               "ml-[-6px] mt-[-3px]")})
      (ui/button
        (e/fn []
          (swap! !show-cutoff not))
        (dom/props {:class "w-3 font-bold mt-[-6.5px] h-6"})
        (dom/text "."))
      (dom/div
        (dom/props {:class "ml-[1px] mt-[-10px] mb-[-8px] text-xs breadcrumbs"})
        (dom/ul
          (let [running-history (e/server (e/watch !running-history))
                ordered-ancestor-ids
                (e/server (vec (map #(:db/id (d/entity db %))
                                    (db/get-ancestor-task-ids db selected-id))))
                ;; breadcrumbs-task-ids
                bc-task-ids     (take-last (if show-cutoff
                                             max-cutoff cutoff)
                                           ;; remove sequential dups
                                           (->> running-history
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
                                      (u/tw
                                       "hover:underline"
                                       (when (if (= selected-id running-id)
                                               is-last
                                               (= selected-id bc-task-id))
                                         "underline"))}))))))))))

(e/defn SelectedStatus []
  (let [ancestor-task-ids   (e/server (db/get-ancestor-task-ids db running-id))
        descendant-task-ids (e/server (db/get-descendant-task-ids db running-id))]
    (dom/div
      (dom/props {:class "text-sm mt-[2px]"})
      (cond (u/in? ancestor-task-ids selected-id)
            (dom/div
              (dom/text "Ancestor of currently running ")
              (SelectTaskButton. running-id
                                 {:class "text-xs italic"}))
            (and
             running-id           ; we should have something running
             (= selected-id running-id))
            (let [duration (e/server
                            (int
                             (/ (- e/system-time-ms running-start)
                                1000)))]
              (if (= duration 0)
                (dom/text "Starting "
                          (e/server
                           (-> (d/entity db selected-id)
                               :task/name)))
                (dom/text "Elapsed for " duration " s")))
            (u/in? descendant-task-ids selected-id)
            (dom/div
              (dom/text "Descendant of currently running ")
              (SelectTaskButton. running-id
                                 {:class "text-xs italic"}))))))

(e/defn RunButtons []
  (let [is-running (= running-id selected-id)]
    (dom/div
      (dom/props {:class "flex gap-2"})
      (ui/button
        (e/fn []
          (e/server
           (if is-running
             (stop-running-task)
             (run-selected-task))))
        (dom/props {:class (u/tw
                            "btn-[* xs] block bg-base-300 animation-none hover:bg-base-100")})
        (dom/text
         (if is-running "Stop" "Start")))
      (when is-running
        (ui/button
          (e/fn []
            (e/server
             (reset! !running-id nil)
             (reset! !running-start nil)
             (swap! !running-history drop-last)))
          (dom/props {:class (u/tw
                              "btn-[* xs] block bg-base-300 animation-none hover:bg-base-100")})
          (dom/text "Cancel"))))))

(e/defn SelectedPanel []
  (dom/div
    (dom/props {:class "grow p-2 sm:ml-2 rounded bg-base-200 sm:min-h-full"})
    (dom/div
      (Breadcrumbs.))
    (if selected-id
      (dom/div
        (dom/div
          (dom/props {:class "text-lg"})
          (dom/text (e/server
                     (-> (d/entity db selected-id)
                         :task/name))))
        (dom/div
          (dom/props {:class "ml-2 mt-[1px]"})
          (dom/div
            (dom/props {:class "mb-[5px]"})
            (RunButtons.))
          (SelectedStatus.)
          (dom/div
            (e/for [[start end] (e/server
                                 (reverse
                                  (sort-by
                                   first
                                   (map #(vector (:interval/start %)
                                                 (:interval/end %))
                                        (:task/interval (d/entity db selected-id))))))]
              (dom/div
                (dom/text
                 (u/millis-to-date-format start)
                 " ~ "
                 (u/millis-to-date-format end)))))))
      (dom/text "Select any task."))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (dom/div
        (dom/props {:class "m-10 sm:flex h-fit min-w-[14rem]"})
        (TasksPanel.)
        (SelectedPanel.)
        )))))
