(ns app.reactive-render
  #?(:cljs (:require-macros [app.reactive-render :refer [atomize-form]]))
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [app.tailwind :refer [tw]]           ; tw is a useful fn for grouping utility classes
   [clojure.walk :as walk]
   [clojure.string :as str]
   [zprint.core :as zp]))

(defn extract-then-atomize-let-decls
  "Extracts the let declarations from form and \"atomize\" them. For
  instance, given (let [a 1 b 2] ...), the atomized let decls would be
  [!a (atom 1), a (e/watch !a), !b (atom 2), b (e/watch !b)]."
  [form]
  (letfn [(subexpressions [form]
            ;; all subexpressions
            (cond
              (not (coll? form)) '()
              :else
              (cons form (mapcat subexpressions form))))
          (extract-let-decls [form]
            ;; get let decls from form
            (if (and (list? form) (= 'let (first form)))
              (second form)
              nil))
          (atomize-decls [decls]
            ;; turn decls into atomized decls
            (vec
             (apply concat
                    (map (fn [[sym val]]
                           [(symbol (str "!" (name sym))) `(atom ~val)
                            sym `(e/watch ~(symbol (str "!" (name sym))))])
                         (partition 2 decls)))))]
    (->> (subexpressions form)
         (map extract-let-decls)
         (remove nil?)
         (map atomize-decls)
         (apply concat)
         vec)))

(defn placeholder-split-and-transform
  "Takes s, which has placeholders prefixed with % for let
  declarations, e.g. (let [%x 1 %y 2]). We split s into a list so that
  everything that is not in let declaration is wrapped with dom/text.
  Placeholders along with their values are transformed using transform-fn."
  [s transform-fn]
  (let [;; match declaration variable and value pair (e.g. "%x 1"), or just text without %
        decl-var-and-value-pattern
        #"(%[^()\[\]\"\s]+ [^()\[\]\"\s]+)|[^%]+"
        ])
  (->>
   (re-seq #"(%[^()\[\]\"\s]+ [^()\[\]\"\s]+)|[^%]+" s)
   ;; re-seq produces a list of two-elem vectors
   (map (fn [[x decl?]]
          (cond decl?
                (let [[var value-str] (str/split (subs x 1) #" ")]
                  (transform-fn (symbol var)
                                value-str))
                :else
                `(dom/text ~x))))))

(defn insert-controls
  "Given transform-fn (which defines how the control should look:
  slider, button, etc.), insert controls (generated through
  transform-fn) after let declarations. Returns a list of dom/text and
  control elements."
  [form transform-fn]
  ;; we can probably do some validation of format string w/ count of controls
  (letfn [(map-first-and-every-other [f coll]
            (map-indexed (fn [idx item] (if (even? idx) (f item) item))
                         coll))]
    (-> (walk/postwalk
         (fn [form]
           (cond (and (list? form) (= 'let (first form)))
                 (let [bindings (second form)
                       body     (drop 2 form)]
                   `(~'let ~(vec (map-first-and-every-other
                                  (fn [sym]
                                    (symbol (str "%" sym)))
                                  bindings))
                     ~@body))
                 (list? form)
                 form
                 :else
                 form))
         form)
        zp/zprint-str
        ;; at this point, we have string with placeholders
        (placeholder-split-and-transform transform-fn)
        )))


(defn remove-all-let-decls
  "A helper function which removes all let declarations from a form."
  [form]
  (letfn [(remove-let-decls [form]
            (drop 2 form))]
    (->
     (walk/prewalk
      (fn [form]
        (if (list? form)
          (mapcat
           (fn [subform]
             (cond (and (list? subform) (= (first subform) 'let))
                   (if (= (count subform) 2)
                     nil
                     (drop 2 subform))
                   :else
                   (list subform)))
           form)
          form))
      (list form))
     first)))

(e/def container-node)                  ; unused
#?(:clj
   (defmacro atomize-form
     "Creates a display where one pane is the evaluation of the form
     (i.e. rendered dom) and the other is the form itself. The catch
     is that these two panes are connected; the form pane has controls
     for each let binding, and changing these controls changes the
     rendering pane."
     [form]
     `(binding [container-node dom/node] ; unused
        (dom/div
          (dom/props {:class (tw "[&>*]:[p-4 rounded-lg]"
                                 "flex-[* col] gap-4 m-10 text-2xl p-10 rounded-xl")})
          (let ~(extract-then-atomize-let-decls form)
            (dom/div
              (dom/props {:class "bg-base-200 w-64"})
              ~(remove-all-let-decls form))
            (dom/div
              (dom/props {:class "bg-base-200 text-md w-fit"})
              (dom/pre
               ~@(insert-controls
                  form
                  (fn [x value-string]
                    (let [!x       (symbol (str "!" (name x)))
                          value    (read-string value-string)
                          interval 50]
                      `(let [!showing (atom false)
                             showing  (e/watch !showing)]
                         (dom/div
                           (dom/props {:class "inline-block mb-4"})
                           (ui/range
                            ~x
                            (e/fn [v]
                              (reset! ~!x v))
                            (dom/props {:min   ~(- value interval)
                                        :max   ~(+ value interval)
                                        :class (tw "range-[* sm] w-32")
                                        })
                            (dom/on "click" (e/fn [e]
                                              (.stopPropagation e))))
                           (dom/div
                             (dom/props
                              {:class "w-32 flex justify-between text-xs px-2 absolute"})
                             (dom/span (dom/text ~(- value interval)))
                             (dom/span (dom/text ~value))
                             (dom/span (dom/text ~(+ value interval)))))
                         (dom/text " " ~(name x)))))))))))))

(e/defn ReactiveRenderApp []
  (atomize-form
   (let [w 20 h 20]
     (svg/svg
      (dom/props {:width w :height h
                  :class (tw "border-[2 black]")})
      (let [x1           5 x2 5
            y1           0 y2 10
            stroke-width 1]
        (svg/line (dom/props {:x1     x1       :x2           x2
                              :y1     y1       :y2           y2
                              :stroke "black", :stroke-width stroke-width})))))))
