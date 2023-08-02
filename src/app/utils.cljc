;; {:nextjournal.clerk/visibility {:code :hide :result :hide}}

(ns app.utils
  (:require
   #?(:cljs
      [goog.string :as gstring])
   #?(:cljs
      [goog.string.format :as format])
   [hyperfiddle.rcf :refer [tests]]
   [lambdaisland.regal :as regal]
   [instaparse.core :as insta]
   [clojure.string]))

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

;; Note: `word` here is a non-bracketed non-whitespace string.
(def tailwind-parser
  (insta/parser
   "S = (item <space>)* item
item = (no-group | grouped)
no-group = word
grouped = (colon-prefix | dash-prefix) <'['> ((subitem <space>)* subitem)? <']'>
colon-prefix = (('[' colon-prefix-word ']' | colon-prefix-word) ':') +
colon-prefix-word = #'[^:\\[\\]\\s]+'
dash-prefix = (('[' colon-prefix-word ']' | colon-prefix-word) ':')* #'[^-:\\[\\]\\s]+' '-'
subitem = (word | itself)
word = #'[^\\[\\]\\s]+'
itself = '*'
space = #'\\s+'
"))

(defn on-args [f]
  (fn [& args]
    (f args)))

(def tailwind-valid-group-prefixes
  ["border" "grid" "flex" "input" "btn" "font" "text"])
(defn tailwind-generator [tree]
  (->> tree
       (insta/transform
        {:word              str
         :colon-prefix-word str
         :subitem           (on-args first)
         :item              (on-args first)
         :no-group          str
         :grouped           (fn [& args]
                              (let [prefix-type     (ffirst args)
                                    prefix          (apply str (rest (first args)))
                                    contains-itself (some #(= (first %) :itself)
                                                          (rest args))
                                    subitems        (remove #(= (first %) :itself)
                                                            (rest args))]
                                (if (and (= prefix-type :dash-prefix)
                                         (= (count subitems) 1)
                                         (not
                                          (re-matches
                                           (re-pattern
                                            (str
                                             ".*("
                                             (clojure.string/join
                                              "|"
                                              tailwind-valid-group-prefixes)
                                             ")-$"))
                                           prefix)))
                                  (str prefix "[" (first subitems) "]")
                                  (clojure.string/join
                                   " "
                                   (concat
                                    (when contains-itself
                                      (list (subs* prefix 0 -1)))
                                    (map (fn [subitem]
                                           (str prefix
                                                subitem))
                                         subitems))))))
         :S                 (on-args #(clojure.string/join " " %))
         })))

(defn tailwind-compiler [string]
  (tailwind-generator
   (tailwind-parser string)))

(comment
  (tailwind-compiler "focus:hover:grid-[* p-4 rounded-2xl font-mono]")
  (tailwind-compiler "[&>*]:[p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid-[* p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid hover:x")
  (tailwind-compiler "hover:[p-4 rounded-2xl font-mono]")
  (tailwind-compiler "grid-[]")
  (tailwind-compiler "hover:[]"))

(defn tw [& args]
  (tailwind-compiler
   (clojure.string/join " " (remove nil? args))))

;; {:nextjournal.clerk/visibility {:code :hide :result :hide}}

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
