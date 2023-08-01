(ns app.push
  (:import [hyperfiddle.electric Pending]
           #?(:clj [java.time Duration Instant]))
  (:require
   ;; #?(:clj [datalevin.core :as d])
   #?(:clj [datascript.core :as d])
   [app.utils :as u]
   #?(:clj [app.tx :as tx])
   #?(:clj [app.db :as db])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [hyperfiddle.rcf :refer [tests]]
   [contrib.str :refer [empty->nil]]))

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
                      :interval/start {}
                      :interval/end   {}
                      })
     (defonce !conn
       (d/create-conn schema)
       ;; (d/get-conn "/tmp/datalevin/mygdbd" schema)
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
         (reset! !running-start nil))
       )

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
       ;; (tests
       ;;  (d/get-ancestor-task-ids @!conn 5) := [1 2]
       ;;  (set (d/get-descendant-task-ids @!conn 2)) := (set [4 5 7 8]))
       )

     (comment
       (d/transact! !conn [{:db/id 1 :task/name "Software"}])
       (d/transact! !conn [{:db/id 26 :task/name "Misc"}])
       (d/transact! !conn [{:db/id 1 :task/subtask [{:db/id 2 :task/name "Push"}
                                                    {:db/id 3 :task/name "Slix"}
                                                    {:db/id 9 :task/name "Emacs Extension"}]}])
       (d/transact! !conn [{:db/id 2 :task/subtask [{:db/id 4 :task/name "Learn Libraries"}
                                                    {:db/id 5 :task/name "Code"}
                                                    {:db/id 20 :task/name "Design"}
                                                    {:db/id 22 :task/name "Think"}]}])
       (d/transact! !conn [{:db/id 4 :task/subtask [{:db/id 8 :task/name "Electric"}
                                                    {:db/id 7 :task/name "Missionary"}
                                                    {:db/id 10 :task/name "Portfolio"}
                                                    {:db/id 23 :task/name "Lexical"}]}])
       (d/transact! !conn [{:db/id 5 :task/subtask [{:db/id 11 :task/name "Compose"}
                                                    {:db/id 12 :task/name "Test"}
                                                    {:db/id 13 :task/name
                                                     "Refactor"}
                                                    {:db/id 19 :task/name
                                                     "Navigate"}
                                                    {:db/id 27 :task/name
                                                     "Debug"}]}])
       (d/transact! !conn [{:db/id        14 :task/name "Break"
                            :task/subtask [{:db/id 15 :task/name "Netflix"}
                                           {:db/id 16 :task/name "YouTube"}
                                           {:db/id 17 :task/name "Free Break"}]}])
       (d/transact! !conn [{:db/id 1 :task/subtask [{:db/id 18 :task/name "Cursory Look"}]}])
       (d/transact! !conn [{:db/id 20 :task/subtask [{:db/id 21 :task/name "Introspectively"}]}])
       (d/transact! !conn [{:db/id 22 :task/subtask [{:db/id 24 :task/name "Think about how to incorporate Lexical"}
                                                     {:db/id 25 :task/name "Philosophically"}]}])
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
  (let [toggled
        (if (u/in? temp-show-task-ids task-id)
          false
          (e/server (:task/toggled (d/entity db task-id))))
        ;; (e/watch !toggled)
        ]
    (ui/button
      (e/fn []
        (js/console.log "Toggling " task-id)
        (e/server
         (tx/transact! !conn [{:db/id task-id
                               :task/toggled
                               (not (if (some #(= % task-id) (e/client
                                                              @!temp-show-task-ids))
                                      false
                                      (:task/toggled (d/entity @!conn task-id))))}]))
        (reset! !temp-show-task-ids nil))
      (dom/props {:class "ml-1 btn btn-xs w-fit px-1"})
      (dom/on "click" (e/fn [e]
                        (.stopPropagation e)))
      (if toggled
        (svg/svg (dom/props {:width  10
                             :height 10})
                 (SVGVerticalLine.)
                 (SVGHorizontalLine.))
        (svg/svg (dom/props {:width  10
                             :height 10})
                 (SVGHorizontalLine.))))))

(e/def TaskList)
(e/defn TasksPanel []
  (dom/div
    (dom/props {:class "grow sm:max-w-xs min-h-[14rem] sm:min-h-[18rem] min-h-full sm:h-none mb-2 sm:mb-0 p-2 rounded bg-base-200"})
    (let [!editing (atom false), editing (e/watch !editing)]
      (dom/div
        (dom/props {:class "text-xl font-bold flex items-center"})
        (ui/button
          (e/fn [] (e/server (reset! !selected-id running-id)))
          (dom/text "Day 5"))
        (ui/button
          (e/fn [] (swap! !editing not))
          (dom/props {:class "ml-2 btn mt-[1px] btn-xs bg-base-100"})
          (dom/text (if editing "stop editing" "edit"))))
      (dom/div
        (binding
            [TaskList
             (e/fn [task-ids]
               (e/for [[task-id is-last]
                       (map-indexed (fn [idx task-id]
                                      [task-id (= idx (dec (count task-ids)))])
                                    task-ids)]
                 (let [task        (e/server
                                    (:task/name (d/entity db task-id)))
                       subtask-ids (e/server
                                    (sort (map :db/id (:task/subtask (d/entity db task-id)))))]
                   (dom/div
                     (dom/props
                      {:class (str "flex "
                                   ;; don't use cond
                                   (when (= task-id running-id)
                                     "font-bold ")
                                   (when (= task-id selected-id)
                                     "underline "))})
                     (ui/button
                       (e/fn []
                         ;; for disabling double-click
                         ;; (e/server (reset! !selected-id task-id))
                         (if (= selected-id task-id)
                           (e/server (run-selected-task))
                           (e/server (reset! !selected-id task-id))))
                       (dom/props {:class "w-full text-left flex"})
                       (dom/text task)
                       (when (seq subtask-ids)
                         (dom/div
                           (dom/props {:class "no-underline"})
                           (Toggle. task-id)))))
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
      (dom/props {:class (str "flex text-xs "
                              "ml-[-4px]")})
      (ui/button
        (e/fn []
          (swap! !show-cutoff not))
        (dom/props {:class "w-3 mt-[-8px] font-bold"})
        (dom/text "."))
      (dom/div
        (dom/props {:class "ml-[1px] mt-[-10px] mb-[-8px] text-xs breadcrumbs"})
        (dom/ul
          (let [running-history      (e/server (e/watch !running-history))
                ordered-ancestor-ids
                (e/server (vec (map #(:db/id (d/entity db %))
                                    (db/get-ancestor-task-ids db selected-id))))
                breadcrumbs-task-ids (->> running-history
                                          (partition-by identity)
                                          (map first))]
            (e/for [breadcrumbs-task-id
                    (if show-cutoff
                      (take-last max-cutoff breadcrumbs-task-ids)
                      (take-last cutoff breadcrumbs-task-ids))]
              (dom/li
                (SelectTaskButton. breadcrumbs-task-id
                                   {:class
                                    (str
                                     "hover:underline "
                                     (when (= breadcrumbs-task-id selected-id)
                                       "underline"))})))))))))

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

(e/defn RunButton []
  (if (= running-id selected-id)
    (ui/button
      (e/fn []
        (e/server
         (stop-running-task)))
      (dom/props {:class "btn btn-xs block"})
      (dom/text "Stop"))
    (ui/button
      (e/fn []
        (e/server
         (run-selected-task)))
      (dom/props {:class "btn btn-xs block"})
      (dom/text "Start"))))

(e/defn SelectedPanel []
  (dom/div
    (dom/props {:class "grow p-2 sm:ml-2 rounded bg-base-200 sm:min-h-full"})
    (dom/div
      (Breadcrumbs.))
    (dom/div
      (if selected-id
        (dom/div
          (dom/div
            (dom/props {:class "text-lg"})
            (dom/text (e/server
                       (-> (d/entity db selected-id)
                           :task/name))))
          (RunButton.))
        (dom/text "Select any task."))
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
             (u/millis-to-date-format end))))))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (dom/div
        (dom/props {:class "m-10 sm:flex h-fit min-w-[14rem]"})
        ;; (dom/text temp-show-task-ids)
        (TasksPanel.)
        (SelectedPanel.))))))
