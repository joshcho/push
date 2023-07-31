(ns app.push
  (:import [hyperfiddle.electric Pending])
  (:require
   ;; #?(:clj [datalevin.core :as d])
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [contrib.str :refer [empty->nil]]
   ;; #?(:cljs )
   ))

(defn in? [list elem]
  (some #(= % elem) list))

#?(:clj
   (do
     (defonce schema {:task/subtask {:db/cardinality :db.cardinality/many
                                     :db/valueType   :db.type/ref}
                      :task/name    {:db/unique :db.unique/identity}})
     (defonce !conn
       (d/create-conn schema)
       ;; (d/get-conn "/tmp/datalevin/mygdbd" schema)
       )
     (defonce !running-id (atom nil))
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
       (d/q '[:find [?ancestor ...]
              :in $ % ?task
              :where
              (ancestor ?ancestor ?task)]
            db rules task-id))

     (defn task-subtasks [db task-id]
       (-> (d/entity db task-id)
           :task/subtask))
     (comment
       (ancestor-task-ids @!conn 5)
       (d/transact! !conn [{:db/id 1 :task/name "Dream"}])
       (d/transact! !conn [{:db/id 1 :task/subtask [{:db/id 2 :task/name "Sleep"}
                                                    {:db/id 3 :task/name "Dream More"}]}])
       (d/transact! !conn [{:db/id 2 :task/subtask [{:db/id 4 :task/name "Sleep Well"}
                                                    {:db/id 5 :task/name "Sleep Beautiful"}]}])
       (ancestor-task-ids @!conn 3)
       (d/transact! !conn [{:db/id 4 :task/subtask [{:db/id 8 :task/name "Sleep Oh"}
                                                    {:db/id 7 :task/name "Sleep Wows"}]}]))))

(defn map-is-last
  "Go from a list to a list of pairs where the first is the item and the second is whether the item is the last item in the list."
  [xs]
  (map vector xs
       (map (partial = (- (count xs)
                          1))
            (range))))

(e/def db)
(e/def running-id (e/server (e/watch !running-id)))

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
    (dom/props {:class "grow max-w-xs p-2 rounded bg-base-200"})
    (let [!editing (atom false), editing (e/watch !editing)]
      (dom/div
        (dom/props {:class "text-xl font-bold"})
        (dom/text "Tasks List")
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
                     toggled     (e/watch !toggled)]
                 (dom/div
                   (dom/props
                    {:class (str "flex " (when (= task-id running-id)
                                           "font-bold"))})
                   (ui/button
                     (if (= @!selected-id task-id)
                       (e/server (reset! !running-id task-id))
                       (reset! !selected-id task-id))
                     (dom/props {:class "w-full text-left flex pl-1"})
                     (dom/text task)
                     (when (seq subtask-ids)
                       (Toggle. !toggled))))
                 (when-not toggled
                   (dom/div
                     (dom/props {:class "ml-2"})
                     (TaskList. subtask-ids))))))]
        (TaskList. (e/server (get-root-task-ids db)))))))

(e/defn SelectedPanel [!selected-id]
  (let [selected-id       (e/watch !selected-id)
        ancestor-task-ids (e/server (get-ancestor-task-ids db running-id))]
    (dom/div
      (dom/props {:class "grow p-2 ml-2 rounded bg-base-200 h-64"})
      (if selected-id
        (dom/div
          (dom/div
            (dom/props {:class "text-xs breadcrumbs"})
            (dom/ul
              (let [ordered-ancestor-ids
                    (e/server (vec (reverse
                                    (map #(:db/id (d/entity db %))
                                         (get-ancestor-task-ids db selected-id)))))]
                (e/for [breadcrumbs-task-id (conj ordered-ancestor-ids selected-id)]
                  (dom/li
                    (ui/button
                      (e/fn []
                        (reset! !selected-id breadcrumbs-task-id))
                      (dom/text (e/server
                                 (-> (d/entity db breadcrumbs-task-id)
                                     :task/name)))))))))
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

        (dom/text "Select any task."))
      (when (in? ancestor-task-ids selected-id)
        (dom/div
          (dom/props {:class "text-sm"})
          (dom/text "Counting via ")
          (ui/button
            (e/fn []
              (reset! !selected-id running-id))
            (dom/props {:class "mt-1 text-xs italic"})
            (dom/text
             (e/server (:task/name (d/entity db running-id))))))))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (let [!selected-id (atom nil)]
        (dom/div
          (dom/props {:class "m-10 flex"})
          (TasksPanel. !selected-id)
          (SelectedPanel. !selected-id)))))))
