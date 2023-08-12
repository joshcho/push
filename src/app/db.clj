(ns app.db
  (:require
   [datalevin.core :as d]
   [contrib.data :refer [nil-or-empty?]]
   ;; [datascript.core :as d]
   ))

;; (defn get-root-task-ids [db]
;;   (->>
;;    (d/q '[:find [(pull ?e [:db/id :task/name :task/subtask]) ...]
;;           :where
;;           [?e :task/name ?n]
;;           (not [?f :task/subtask ?e])]
;;         db)
;;    (map :db/id)
;;    sort))

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

(defn get-notes-from-task-id [db task-id]
  (->> (d/entity db task-id)
       :task/interval
       (remove #(nil-or-empty? (:interval/note %)))
       (map #(select-keys % [:db/id
                             :interval/start
                             :interval/end
                             :interval/note]))))

(defn get-descendant-task-ids [db task-id]
  (->
   (d/q '[:find [?descendant ...]
          :in $ % ?task
          :where
          (ancestor ?task ?descendant)]
        db rules task-id)
   reverse))

#_(comment
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
    )
