(ns app.utils
  (:require
   [hyperfiddle.rcf :refer [tests]]))

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

(defn in? [list elem]
  (some #(= % elem) list))

(defn concatv [& args]
  (vec
   (apply concat args)))

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

#?(:cljs
   (defn millis-to-date-format [millis]
     (let [date    (js/Date. (+ millis (* 9 60 60 1000))) ; KST is UTC+9
           month   (.getUTCMonth date)
           day     (.getUTCDate date)
           hours   (.getUTCHours date)
           minutes (.getUTCMinutes date)]
       (str (inc month) "/" day " " hours ":" minutes))))
