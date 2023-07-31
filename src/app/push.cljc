(ns app.push
  (:import [hyperfiddle.electric Pending])
  (:require
   ;; #?(:clj [datalevin.core :as d])
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [hyperfiddle.rcf :refer [tests]]
   [contrib.str :refer [empty->nil]]
   ;; #?(:cljs )
   ))

(comment
  (hyperfiddle.rcf/enable!))

(defn make-history-atom [src-atom]
  "Return an atom that keeps the history of src-atom."
  (let [history-atom (atom (if @src-atom
                             [@src-atom]
                             []))]
    (add-watch src-atom :history
               (fn [_ _ old new]
                 (when-not (= old new)
                   (swap! history-atom #(conj % new)))))
    history-atom))

(comment
  (tests
   (let [test-atom (atom 0)
         history-atom
         (make-history-atom test-atom)]
     (swap! test-atom inc)
     (swap! test-atom inc)
     (swap! test-atom inc)
     @history-atom)
   := [0 1 2 3]
   (let [test-atom (atom nil)
         history-atom
         (make-history-atom test-atom)]
     (reset! test-atom 0)
     (swap! test-atom inc)
     @history-atom)
   := [0 1]))

#?(:clj
   (do
     (defonce schema {:task/subtask      {:db/cardinality :db.cardinality/many
                                          :db/valueType   :db.type/ref}
                      :task/name         {:db/unique :db.unique/identity}
                      :task/interval     {:db/cardinality :db.cardinality/many
                                          :db/valueType   :db.type/ref}
                      ;; for datalevin, {:db/valueType :db.type/instant}
                      :task/active-start {}
                      :interval/start    {}
                      :interval/end      {}
                      })
     (defonce !conn
       (d/create-conn schema)
       ;; (d/get-conn "/tmp/datalevin/mygdbd" schema)
       )
     (defonce !running-id (atom nil))
     (defonce !running-history (make-history-atom !running-id))))
(e/def db)
(e/def running-id (e/server (e/watch !running-id)))

(defn in? [list elem]
  (some #(= % elem) list))

#?(:clj
   (do
     (defn get-root-task-ids [db]
       (->>
        (d/q '[:find [(pull ?e [:db/id :task/name :task/subtask]) ...]
               :where
               [?e :task/name ?n]
               (not [?f :task/subtask ?e])]
             db)
        (map :db/id)
        sort))
     (def rules
       '[[(ancestor ?e ?a)
          [?e :task/subtask ?a]]
         [(ancestor ?e ?a)
          [?e :task/subtask ?b]
          (ancestor ?b ?a)]])

     (defn get-ancestor-task-ids [db task-id]
       (->
        (d/q '[:find [?ancestor ...]
               :in $ % ?task
               :where
               (ancestor ?ancestor ?task)]
             db rules task-id)
        reverse))

     (defn get-descendant-task-ids [db task-id]
       (->
        (d/q '[:find [?ancestor ...]
               :in $ % ?task
               :where
               (ancestor ?task ?ancestor)]
             db rules task-id)
        reverse))
     (comment
       ;; (tests
       ;;  (get-ancestor-task-ids @!conn 5) := [1 2]
       ;;  (set (get-descendant-task-ids @!conn 2)) := (set [4 5 7 8]))
       )

     (comment
       (ancestor-task-ids @!conn 5)
       (d/transact! !conn [{:db/id 1 :task/name "Software"}])
       (d/transact! !conn [{:db/id 1 :task/subtask [{:db/id 2 :task/name "Push"}
                                                    {:db/id 3 :task/name "Slix"}
                                                    {:db/id 9 :task/name "Emacs Extension"}]}])
       (d/transact! !conn [{:db/id 2 :task/subtask [{:db/id 4 :task/name "Learn Libraries"}
                                                    {:db/id 5 :task/name "Code"}]}])
       (ancestor-task-ids @!conn 3)
       (d/transact! !conn [{:db/id 4 :task/subtask [{:db/id 8 :task/name "Electric"}
                                                    {:db/id 7 :task/name "Missionary"}
                                                    {:db/id 10 :task/name "Portfolio"}]}])
       (d/transact! !conn [{:db/id 5 :task/subtask [{:db/id 11 :task/name "Compose"}
                                                    {:db/id 12 :task/name "Test"}
                                                    {:db/id 13 :task/name
                                                     "Refactor"}]}])
       (d/transact! !conn [{:db/id        14 :task/name "Break"
                            :task/subtask [{:db/id 15 :task/name "Netflix"}
                                           {:db/id 16 :task/name "YouTube"}
                                           {:db/id 17 :task/name "Free Break"}]}])
       (d/transact! !conn [{:db/id 18 :task/name "Misc"}]))))

(defn map-is-last
  "Go from a list to a list of pairs where the first is the item and the second is whether the item is the last item in the list."
  [xs]
  (map vector xs
       (map (partial = (- (count xs)
                          1))
            (range))))

(comment
  (tests
   (map-is-last [1 2 3]) := '([1 false] [2 false] [3 true])
   (map-is-last nil) := '()))

;; (e/defn Task [task-id editing !editing-map editing-map !selected-id selected-id]
;;   (dom/div (dom/text (e/server (task-name db task-id)))))

(e/defn Toggle [!toggled]
  (let [toggled (e/watch !toggled)]
    (ui/button
      (e/fn []
        (swap! !toggled not))
      (dom/props {:class "ml-1 btn btn-xs"})
      (dom/on "click" (e/fn [e]
                        (.stopPropagation e)))
      (dom/text (if toggled "+" "-")))))

(e/def TaskList)
(e/defn TasksPanel [!selected-id]
  (dom/div
    (dom/props {:class "grow sm:max-w-xs min-h-[14rem] min-h-full sm:h-none mb-2 sm:mb-0 p-2 rounded bg-base-200"})
    (let [!editing (atom false), editing (e/watch !editing)]
      (dom/div
        (dom/props {:class "text-xl font-bold"})
        (ui/button
          (e/fn [] (reset! !selected-id nil))
          (dom/text "Day 5"))
        (ui/button
          (e/fn [] (swap! !editing not))
          (dom/props {:class "ml-1 btn btn-xs"})
          (dom/text (if editing "stop editing" "edit"))))
      (binding
          [TaskList
           (e/fn [task-ids]
             (e/for [[task-id is-last] (map-is-last task-ids)]
               (let [!task       (atom (e/server
                                        (:task/name (d/entity db task-id))))
                     task        (e/watch !task)
                     subtask-ids (e/server
                                  (map :db/id (:task/subtask (d/entity db task-id))))
                     !toggled    (atom true)
                     toggled     (e/watch !toggled)
                     selected-id (e/watch !selected-id)]
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
                       (if (= selected-id task-id)
                         (e/server (reset! !running-id task-id))
                         (reset! !selected-id task-id)))
                     (dom/props {:class "w-full text-left flex"})
                     (dom/text task)
                     (when (seq subtask-ids)
                       (dom/div
                         (dom/props {:class "no-underline"})
                         (Toggle. !toggled)))))
                 (when-not toggled
                   (dom/div
                     (dom/props {:class "ml-2"})
                     (TaskList. subtask-ids))))))]
        (TaskList. (e/server (get-root-task-ids db)))))))

(e/defn SelectTaskButton [!selected-id target-id props]
  (ui/button
    (e/fn []
      (reset! !selected-id target-id))
    (dom/props props)
    (dom/text
     (e/server (:task/name (d/entity db target-id))))))

(e/defn SelectedPanel [!selected-id]
  (let [selected-id         (e/watch !selected-id)
        ancestor-task-ids   (e/server (get-ancestor-task-ids db running-id))
        descendant-task-ids (e/server (get-descendant-task-ids db running-id))]
    (dom/div
      (dom/props {:class "grow p-2 sm:ml-2 rounded bg-base-200 h-64"})
      (dom/div
        (dom/div
          (dom/props {:class "mt-[-10px] mb-[-8px] text-xs breadcrumbs"})
          (dom/ul
            (let [running-history (e/server (e/watch !running-history))
                  ordered-ancestor-ids
                  (e/server (vec (map #(:db/id (d/entity db %))
                                      (get-ancestor-task-ids db selected-id))))]
              (e/for [breadcrumbs-task-id
                      (take-last 4 running-history)]
                (dom/li
                  (SelectTaskButton. !selected-id breadcrumbs-task-id
                                     {:class "hover:underline"}))))))
        (if selected-id
          (dom/div
            (dom/div
              (dom/props {:class "text-lg"})
              (dom/text (e/server
                         (-> (d/entity db selected-id)
                             :task/name))))
            (ui/button
              (e/fn []
                (e/server (reset! !running-id
                                  (if (= @!running-id selected-id)
                                    nil selected-id))))
              (dom/props {:class "btn btn-xs block"})
              (dom/text
               (if (= running-id selected-id)
                 "Stop"
                 "Start"))))
          (dom/text "Select any task.")))
      (dom/div
        (dom/props {:class "text-sm"})
        (cond (in? ancestor-task-ids selected-id)
              (dom/div
                (dom/text "Ancestor of currently running ")
                (SelectTaskButton. !selected-id running-id
                                   {:class "text-xs italic"}))
              (and
               running-id               ; we should have something running
               (= selected-id running-id))
              (dom/text "Elapsed for (Mysterious number of seconds)")
              (in? descendant-task-ids selected-id)
              (dom/div
                (dom/text "Descendant of currently running ")
                (SelectTaskButton. !selected-id running-id
                                   {:class "text-xs italic"})))))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (let [!selected-id (atom nil)]
        ;; (add-watch !selected-id :watch)
        (dom/div
          (dom/props {:class "m-10 sm:flex"})
          (TasksPanel. !selected-id)
          (SelectedPanel. !selected-id)))))))
