;; {:nextjournal.clerk/visibility {:code :hide :result :hide}}

(ns app.utils
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

(defn make-history-atom [src-atom]
  "Return an atom that keeps the history of src-atom."
  (let [history-atom (atom (if @src-atom
                             [@src-atom]
                             []))]
    (add-watch src-atom :history
               (fn [_ _ old new]
                 (when (and (not (= old new))
                            new)
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
