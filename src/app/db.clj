(ns app.db
  (:require
   [datascript.core :as d]))

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
