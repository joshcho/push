(ns app.push
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-svg :as svg]
            [contrib.str :refer [empty->nil]]
            #?(:clj [datascript.core :as d])))

#?(:clj
   (do
     (defonce !conn (d/create-conn {}))
     (defonce !running-id (atom nil))))
(e/def db)
(e/def running-id (e/server (e/watch !running-id)))

#(:clj
  (do
    (defn activities [db]
      (->>
       (d/q '[:find [(pull ?e [:db/id :activity/name]) ...]
              :where
              [?e :activity/name ?n]]
            db)
       (sort-by :db/id)))
    (defn activity-name [db id]
      (d/q '[:find ?n .
             :in $ ?e
             :where
             [?e :activity/name ?n]]
           db id))))

(e/defn PushApp []
  (e/server
   (binding [db (e/watch !conn)]
     (e/client
      (let [!selected-id (atom nil), selected-id (e/watch !selected-id)]
        (dom/div
          (dom/props {:class "m-10 flex"})
          (dom/div
            (dom/props {:class "grow max-w-xs p-2 rounded bg-base-200"})
            (let [!new-name    (atom nil),   new-name    (e/watch !new-name)
                  !editing     (atom false), editing     (e/watch !editing)
                  !editing-map (atom {}),    editing-map (e/watch !editing-map)]
              (dom/div
                (dom/props {:class "text-xl font-bold"})
                (dom/text "Activities List")
                (ui/button
                  (e/fn []
                    (swap! !editing not)
                    (when-not (empty? editing-map)
                      (e/server
                       (for [[id val] editing-map]
                         (d/transact! !conn
                                      [(if (= val "")
                                         [:db.fn/retractEntity id]
                                         {:db/id id :activity/name val})])))
                      (reset! !editing-map {})))
                  (dom/props {:class "ml-1 btn btn-xs"})
                  (if editing
                    (dom/text "stop editing")
                    (dom/text "edit")))
                (when-not new-name
                  (ui/button
                    (e/fn [] (reset! !new-name ""))
                    (dom/props {:class "btn btn-xs"})
                    (dom/text "+"))))
              (e/for [{activity-id  :db/id
                       raw-activity :activity/name}
                      (e/server (activities db))]
                (let [!activity (atom raw-activity)
                      activity  (e/watch !activity)]
                  (dom/div
                    (dom/props
                     {:class (str "flex "
                                  (when (= activity-id running-id)
                                    "font-bold"
                                    ;; "font-bold border-black border-l-4 pl-2"
                                    ))})
                    (if editing
                      (ui/input
                       activity (e/fn [v]
                                  (when-not (= activity v)
                                    (swap! !editing-map
                                           assoc activity-id v))
                                  (reset! !activity v))
                       (dom/props {:class "bg-base-100 pl-1 w-48"}))
                      (ui/button
                        (e/fn []
                          (reset! !selected-id activity-id))
                        (dom/props {:class "w-full text-left flex pl-1"})
                        (dom/text activity)
                        (when (and
                               (= activity-id selected-id)
                               (not (= selected-id running-id)))
                          (ui/button
                            (e/fn []
                              (e/server (reset! !running-id selected-id)))
                            (dom/props {:class "ml-1"})
                            (dom/text "%"))))))))
              (when new-name
                (ui/input
                 new-name (e/fn [v]
                            (reset! !new-name v))
                 (dom/on "keydown"
                         (e/fn [e]
                           (when (= "Enter" (.-key e))
                             (e/server
                              (d/transact! !conn [{:db/id -1 :activity/name new-name}]))
                             (reset! !new-name nil))))
                 (dom/props {:class "bg-base-100 pl-1 w-48"}))
                (ui/button
                  (e/fn []
                    (e/server
                     (d/transact! !conn [{:db/id -1 :activity/name new-name}]))
                    (reset! !new-name nil))
                  (dom/props {:class "ml-1 btn btn-xs"})
                  (dom/text "v"))
                (ui/button
                  (e/fn []
                    (reset! !new-name nil))
                  (dom/props {:class "ml-1 btn btn-xs"})
                  (dom/text "x")))))
          (dom/div
            (dom/props {:class "grow p-2 ml-2 rounded bg-base-200"})
            (if selected-id
              (dom/div
                (dom/div
                  (dom/text (e/server (activity-name db selected-id))))
                (ui/button
                  (e/fn []
                    (e/server (reset! !running-id
                                      (if (= @!running-id selected-id)
                                        nil selected-id))))
                  (dom/props {:class "btn btn-xs block mt-1"})
                  (dom/text
                   (if (= running-id selected-id)
                     "Stop"
                     "Start"))))
              (dom/text "Select any activity.")))))))))
