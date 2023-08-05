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
   ;; #?(:clj nextjournal.clerk)
   ;; #?(:cljs
   ;;    [app.render])
   ))

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

;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; (nextjournal.clerk/add-viewers!
;;  [(assoc nextjournal.clerk.viewer/code-block-viewer :render-fn
;;          '(fn [code-string {:as opts :keys [id]}]
;;             [:div.viewer.code-viewer.w-full.max-w-wide {:data-block-id id}
;;              [nextjournal.clerk.render.code/render-code code-string (assoc opts :language "clojure")]]))])

;; just use code mirror
#_{:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/with-viewer
;;   '(fn [code-string {:as opts :keys [id]}]
;;      (let [val       (atom 1)
;;            code-sexp (read-string "(let [val 1] (+ val 2))")]
;;        (vec
;;         `(concat
;;           [:div.viewer.code-viewer.w-full.max-w-wide {:data-block-id id}
;;            [:span "("]
;;            ~@(map #(vector :span %)
;;                   code-sexp)
;;            [:span ")"]]
;;           ))))
;;   "(def fib
;;   (lazy-cat [0 1]
;;             (map + fib (rest fib))))")
;; (let [val 1]
;;   (+ val 2))
#_{:nextjournal.clerk/visibility {:code :show :result :show}}

;; (concatv
;;  [:div.viewer.code-viewer.w-full.max-w-wide]
;;  (map #(vector :p %)
;;       '(+ val 2)))


;; (+ 1 2)

;; (read-string "(+ 1 2)")

#_(nextjournal.clerk/add-viewers!
   [(assoc nextjournal.clerk.viewer/code-block-viewer
           :render-fn '(fn [text opts] your own render logic ))])

#_{:nextjournal.clerk/visibility {:code :hide :result :hide}}

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
