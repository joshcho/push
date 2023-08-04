(ns app.reactive-render
  #?(:cljs (:require-macros [app.reactive-render :refer [atomize-form ident-macro]]))
  (:require
   [app.push :refer [TasksPanel SelectedPanel]]
   #?(:clj [datalevin.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-svg :as svg]
   [app.tailwind :refer [tw]]
   #?(:cljs [cljs.loader])
   #?(:clj [nextjournal.clerk :as clerk])
   [clojure.walk :as walk]
   [clojure.string :as str]
   [zprint.core :as zp]))

;; (def ^::clerk/no-cache cljs-code (slurp "src/render_fns.cljs"))
#_(clerk/eval-cljs-str cljs-code)

#_(clerk/eval-cljs
   '(identity user/electric-main))


#_(clerk/with-viewer
    '(fn [code-string {:as opts :keys [id]}]
       [:div.viewer.code-viewer.w-full.max-w-wide {:data-block-id id}
        ;; [nextjournal.clerk.render.code/render-code
        ;;  code-string
        ;;  (assoc opts :language "clojure")]
        [render-fns/custom-render-test
         code-string
         (assoc opts :language "clojure")]
        ])
    "(let [!a (atom 2)]
  (+ @!a 2))")

(defn extract-then-atomize-let-decls
  "Extracts the let declarations from form and \"atomize\" them. For instance, given (let [a 1 b 2] ...), the atomized let decls would be [!a (atom 1), a (e/watch !a), !b (atom 2), b (e/watch !b)]."
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

(defn in? [list elem]
  (some #(= % elem) list))

(defn placeholder-split-and-transform [s transform-fn]
  (->>
   (re-seq #"(%[^()\[\]\"\s]+ [^()\[\]\"\s]+)|[^%]+" s)
   (map (fn [[x decl?]]
          (cond decl?
                (let [[var value-str] (str/split (subs x 1) #" ")]
                  (transform-fn (symbol var)
                                value-str))
                :else
                `(dom/text ~x))))))

(defn insert-controls [form transform-fn]
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

(defn remove-all-let-decls [form]
  (walk/prewalk
   (fn [form]
     (cond (and (list? form) (= (first form) 'let))
           (if (= (count form) 2)
             nil
             (drop 2 form))
           :else
           form))
   form))

#?(:clj
   (defmacro atomize-form [form]
     `(dom/div
        (dom/props {:class (tw "[&>*]:[p-4 rounded-lg]"
                               "flex-[* col] gap-4 m-10 text-2xl p-10 rounded-xl")})
        (let ~(extract-then-atomize-let-decls form)
          (dom/div
            (dom/props {:class "bg-base-200 w-64"})
            ~(remove-all-let-decls form))
          (dom/div
            (dom/props {:class "bg-base-200 text-md w-96"})
            (dom/pre
             ~@(insert-controls
                form
                (fn [x value-string]
                  `(let [!showing (atom false)
                         showing  (e/watch !showing)]
                     (let [value (int value-string)]
                       (dom/span
                        (ui/range
                         ~(name x)
                         (e/fn [v]
                           (reset! ~(symbol (str "!" x)) v))
                         (dom/props {:min   (- value 50)
                                     :max   (+ value 50)
                                     :value @~(symbol (str "!" x))
                                     :class (str "absolute mt-7 -ml-4 "
                                                 (when-not showing
                                                   "hidden"))}))
                        (ui/button
                          (e/fn []
                            (swap! !showing not))
                          (dom/on "click" (e/fn [e]
                                            (.stopPropagation e)))
                          (dom/b
                           (dom/text ~(name x)))
                          (dom/span
                           (dom/text " " ~x))))))))))))))

(e/defn TestApp []
  (atomize-form
   (let [w 10 h 10]
     (svg/svg
      (dom/props {:width w :height h})
      (let [x1           5   x2 5
            y1           0.2 y2 9.8
            stroke-width 1]
        (svg/line (dom/props {:x1     x1       :x2           x2
                              :y1     y1       :y2           y2
                              :stroke "black", :stroke-width stroke-width})))))))

(e/defn ReactiveRenderApp []
  (TestApp.))
