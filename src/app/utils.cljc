;; {:nextjournal.clerk/visibility {:code :hide :result :hide}}

(ns app.utils
  #?(:cljs (:require-macros [app.utils :refer [make-relay textarea*
                                               make-relay-task
                                               relay-atom
                                               e-relay-atom
                                               selected-task-relay-atom]]))
  (:require
   #?(:cljs
      [goog.string :as gstring])
   #?(:cljs
      [goog.string.format :as format])
   [hyperfiddle.rcf :refer [tests]]
   [lambdaisland.regal :as regal]
   [clojure.string]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]
   #?(:clj [app.tx :as tx])
   ;; #?(:clj nextjournal.clerk)
   ;; #?(:cljs
   ;;    [app.render])
   ))

;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; (m/sleep 800)
;; (m/timeout (m/sleep 1000) 800 :a)
;; (m/? (m/timeout (m/sleep 1000) 800 :a))
;; (m/? (m/timeout (m/sleep 1000) 2000 :a))
;; (m/? (m/timeout
;;       (m/sp (println "one")
;;             :two)
;;       100
;;       :b))

;; (-> (m/sp (println "Let's take a nap")
;;           (str (m/? (m/sleep 900 "Hi "))
;;                (m/? (m/sleep 100 "there"))))
;;     (m/timeout 1001 :a)
;;     m/?)
;; ((m/sp "world")
;;  #(println "Hello" %)
;;  #(println :KO %))
;; (def a-task (m/sleep 3000 :done))
;; (def cancel (a-task #(println :ok %)
;;                     (fn [_] (println :KO))))
;; (cancel)

;; (let [v1 (m/? (m/sp "hi"))
;;       v2 (m/? (m/sp "there"))]
;;   (printf "Read %s from %s%n" v1 v2))
;; (let [[v1 v2] (m/? (m/join
;;                     vector
;;                     (m/sp "hi")
;;                     (m/sp "there")))]
;;   (printf "Read %s from %s%n" v1 v2))
;; (m/seed [1 2 3])
;; (m/zip vector
;;        (m/seed (range 3))
;;        (m/seed [:a :b :c]))
;; (m/zip vector
;;        (m/seed (range 3))
;;        (m/seed [:a :b :c]))

;; ;; (def !input (atom 0))
;; (def some-f (->> (m/ap (let [n (m/?> (m/seed [500 500 500 500]))]
;;                          (m/? (m/sleep n n))))
;;                  (m/reduce (fn [_ c]
;;                              (println (str c)))
;;                            nil)))
;; (m/? some-f)

;; (defn forever [task]
;;   (m/ap (m/? (m/?> (m/seed (repeat task))))))

;; (defn rdv-flow [rdv]
;;   (forever rdv))

;; (defn print-call [t]
;;   (t println println))

;; (defn print-drain [f]
;;   (m/reduce println f))

;; (comment
;;   (def rdv (m/rdv))
;;   (def cancel (print-call (print-drain (rdv-flow rdv))))
;;   (m/? (rdv "val 1"))
;;   (cancel)
;;   (m/? (rdv "val 1")) ;; prints nil val 1, blocks until flow is ready to accept new value
;;   (cancel))
;; (m/?
;;  (->> some-f
;;       (m/reductions (fn [r v]
;;                       (print v)
;;                       r)
;;                     0)
;;       (m/reduce conj)))

;; ;; (m/?
;; ;;  (->> some-f
;; ;;       (m/eduction (map print))
;; ;;       ;; (m/reductions (fn [r _] (inc r)) 0)
;; ;;       ;; (m/relieve {})
;; ;;       ))

;; (defn delay-each [delay input]
;;   (m/ap (m/? (m/sleep delay (m/?> input)))))

;; ;; (m/? (->> (m/ap (let [n (m/?> (m/seed [20 30 40 50]))]
;; ;;                   (m/? (m/sleep n n))))
;; ;;           ;; (m/relieve +)
;; ;;           (delay-each 200)
;; ;;           (m/reduce conj)))

;; ;; (+ 1 2)
;; ;; (m/?
;; ;;  (m/reduce (fn [_ c] (println (str "clicked " c " times."))) nil input-count))

;; {:nextjournal.clerk/visibility {:code :hide :result :hide}}

(defn in? [list elem]
  (some #(= % elem) list))

(defn concatv [& args]
  (vec
   (apply concat args)))

(defn safe-1-based-indexer [coll n]
  (when (< (dec n) (count coll))
    (nth coll (dec n))))
(defn third [coll]
  (safe-1-based-indexer coll 3))
(defn fourth [coll]
  (safe-1-based-indexer coll 4))
(defn fifth [coll]
  (safe-1-based-indexer coll 5))
(defn subs* [s start & [end]]
  (let [s-len (count s)
        start (if (neg? start) (+ s-len start) start)
        end   (if (and end (neg? end)) (+ s-len end) end)]
    (subs s start end)))

#_(nextjournal.clerk/add-viewers!
   [(assoc nextjournal.clerk.viewer/code-block-viewer :render-fn
           '(fn [code-string {:as opts :keys [id]}]
              [:div.viewer.code-viewer.w-full.max-w-wide {:data-block-id id}
               [nextjournal.clerk.render.code/render-code code-string (assoc opts :language "clojure")]]))])

#_{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(clerk/with-viewer
    '(fn [code-string {:as opts :keys [id]}]
       (let [val       (atom 1)
             code-sexp (read-string "(let [val 1] (+ val 2))")]
         (vec
          `(concat
            [:div.viewer.code-viewer.w-full.max-w-wide {:data-block-id id}
             [:span "("]
             ~@(map #(vector :span %)
                    code-sexp)
             [:span ")"]]
            ))))
    "(def fib
  (lazy-cat [0 1]
            (map + fib (rest fib))))")

#_{:nextjournal.clerk/visibility {:code :show :result :show}}


#_(nextjournal.clerk/add-viewers!
   [(assoc nextjournal.clerk.viewer/code-block-viewer
           :render-fn '(fn [text opts] your own render logic ))])

#?(:clj
   (defmacro make-relay-task
     [ref selected-task-id attr]
     `(let [!last-sent          (atom :null)
            !ignore-one         (atom true)
            !selected-id-change (atom true)]
        (e/for-event
         [id# (e/fn [] ~selected-task-id)]
         ;; (js/console.log "Selected task change: " id#)
         (reset! ~ref (e/server (~attr (d/entity ~'db id#))))
         (reset! !selected-id-change true))

        (e/for-event
         [v# (e/fn [] (e/watch ~ref))]
         ;; (js/console.log "Changed " ~(name attr) ":" v#)
         (cond @!ignore-one
               (do
                 ;; (js/console.log "Ignoring one")
                 (reset! !ignore-one false))
               @!selected-id-change
               (reset! !selected-id-change false)
               :else
               (do
                 ;; (js/console.log "Sending to server")
                 (reset! !last-sent v#)
                 (e/server
                  (tx/transact! ~'!conn [{:db/id
                                          (e/snapshot ~selected-task-id)
                                          ~attr v#}])))))

        (e/for-event
         [v# (e/fn [] (e/server (~attr
                                 (d/entity ~'db ~selected-task-id))))]
         (when (and (not (= v# @!last-sent))
                    (not @!selected-id-change))
           ;; (js/console.log "Received " ~(name attr) ":" v#)
           (reset! !ignore-one true)
           (reset! ~ref v#))
         (reset! !selected-id-change false)
         (reset! !last-sent :null)))))

#?(:clj
   (defn sane-me [form]
     (clojure.walk/prewalk (fn [x] (if (qualified-symbol? x)
                                    (symbol (name x))
                                    x))
                           (macroexpand-1 form))))

(defn ignore-first
  "Transducer that ignores the first value in a sequence."
  []
  (fn [rf]
    (let [first? (atom true)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (if @first?
           (do (reset! first? false) result)
           (rf result input)))))))

#?(:clj
   (do
     (defmacro relay-atom [server-query server-effect]
       `(let [!relay#            (atom (e/server
                                        (e/snapshot ~server-query)))
              !send-count#       (atom 0)
              !stop-propagation# (atom false)]
          (e/for-event
           [v# (->> (e/fn [] (e/watch !relay#))
                    (m/eduction (remove e/failure?)
                                (ignore-first)))]
           ( ;; js/console.log
            identity
            (new
             (e/fn [!send-count# !stop-propagation#]
               (if @!stop-propagation#
                 (reset! !stop-propagation# false)
                 (do
                   ;; (js/console.log "sending" v#)
                   (swap! !send-count# inc)
                   (e/server (new ~server-effect v#)))))
             !send-count# !stop-propagation#)))
          (e/for-event
           [v# (->> (e/fn []
                      (e/server ~server-query))
                    (m/eduction (remove e/failure?)))]
           (new
            (e/fn [!send-count#]
              (if (<= @!send-count# 0)
                (do
                  ;; (js/console.log "received change" v#)
                  (reset! !stop-propagation# true)
                  (reset! !relay# v#))
                (do
                  ;; (js/console.log "received what was sent" v#)
                  (swap! !send-count# dec))))
            !send-count#))
          !relay#))
     (defmacro e-relay-atom [e-id attr]
       `(relay-atom
         (~attr
          (d/entity ~'db ~e-id))
         (e/fn [v#]
           (tx/transact! ~'!conn
                         [{:db/id (e/snapshot ~e-id)
                           ~attr  v#}]))))
     (defmacro selected-task-relay-atom [attr]
       `(e-relay-atom ~'selected-task-id ~attr))))

(comment
  ;; usage, do this in electric functions
  (let [!value (relay-atom
                (:some/value (d/entity db 42)) ; server query
                (fn [v]                         ; server effect
                  (d/transact! !conn [{:db/id      42
                                       :some/value v}])))
        value  (e/watch !value) ; if query is changed in another session, !value is also updated
        ]
    (ui/button
      (e/fn []
        (if (= value "A")
          (reset! !value "B")           ; triggers server change
          (reset! !value "A")           ; triggers server change
          ))
      (dom/text value))))

(comment
  (let [!value (e-relay-atom 42 :some/value)
        value  (e/watch !value)]
    (ui/button
      (e/fn []
        (if (= value "A")
          (reset! !value "B")
          (reset! !value "A")
          ))
      (dom/text value))))

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

(defn compare-with-previous
  "Transducer that compares each item in a sequence with the previous
  one, returning a pair [item, equals-previous?]. For the first item,
  equals-previous? is nil. Optionally takes a keyfn that is used to
  transform each item before comparison."
  ([]
   (compare-with-previous identity))
  ([keyfn]
   (let [prev (atom nil)]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [keyed-input (keyfn input)
                output      (if (nil? @prev)
                              [input nil]
                              [input (= keyed-input @prev)])]
            (reset! prev keyed-input)
            (rf result output))))))))

#?(:cljs (defn value [^js e] (.-target.value e))) ; workaround inference warnings, todo rename

#?(:clj
   (do
     (defmacro control* [event-type parse unparse v V! setter & body]
       `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                                                      (some->> (~parse e#) (new ~V!)))]
          ;; (dom/style {:background-color (when (= ::e/pending state#) "yellow")})
                                        ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
          (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
            (~setter dom/node (~unparse v#))) ; js coerce
          ~@body
          (case state# (::e/pending ::e/failed) (throw v#) (::e/init ::e/ok) v#)))
     (defmacro textarea* [v V! & body]
       `(dom/textarea
         (control* "input" value identity ~v ~V! dom/set-val ~@body)))))
