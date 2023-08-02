(ns app.reactive-render
  (:require
   [app.push :refer [TasksPanel SelectedPanel]]
   #?(:clj [datalevin.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [app.tailwind :refer [tw]]))

(e/defn TestApp []
  (dom/div
    (dom/props {:class "m-10 sm:flex h-fit [&>*]:min-w-[14rem] grid gap-4"})
    (let [!var2 (atom 2)
          var2  (e/watch !var2)]
      (dom/div
        (dom/props {:class "bg-base-200 rounded-lg p-4"})
        (dom/text "cbdc")
        (dom/div
          (let [a var2]
            (dom/text a "abcd" a))))
      (dom/pre
       (dom/props {:class "bg-base-200 rounded-lg p-4"})
       (dom/text "(dom/div
  (dom/text \"cbdc\")
  (dom/div
    (let [")
       (let [!show-control (atom false)
             show-control  (e/watch !show-control)]
         (dom/span
          (dom/span
           (dom/props {:class "hover:cursor-pointer"})
           (dom/on "click" (e/fn [e]
                             (swap! !show-control not)
                             ;; (.preventDefault e)
                             ))
           (dom/text "a"))
          (when show-control
            (dom/div
              (dom/props {:class "absolute"})
              (dom/text "HELLO")))))
       (dom/text " 2]
      (dom/text a \"abcd\" a))))")))))

(e/defn ReactiveRenderApp []
  (TestApp.))

#_(dom/div
    (dom/props {:class "p-4 bg-base-200 rounded-lg "})
    (dom/div
      (dom/text "cbdc"))
    (let [a 2]
      (dom/text "abcd" a)))
#_(dom/pre
   (dom/props {:class "p-4 bg-base-200 rounded-lg"})
   (dom/text "(dom/div
  ")
   (dom/span
    (dom/props {:class (tw "hover:[text-red-700 cursor-pointer]")})
    (dom/text "("))
   (dom/text
    "dom/props {:class \"p-4 bg-base-200 rounded-lg\"}")
   (dom/span
    (dom/props {:class (tw "hover:[text-red-700 cursor-pointer]")})
    (dom/text ")"))
   (dom/text
    "
  (dom/div
    (dom/text \"cbdc\"))
  (let [a 2]
    (dom/text \"abcd\" a)))"
    ))

'(dom/div
   (dom/text "cbdc")
   (let [a 2]
     (dom/text "abcd" a)))

'(let [!var1 (atom "cbdc")
       var1  (e/watch !var1)
       !var2 (atom 2)
       var2  (e/watch !var2)
       !var3 (atom "abcd")
       var3  (e/watch !var3)]
   (dom/div
     (dom/text var1)
     (let [a var2]
       (dom/text var3 var2)))
   (dom/pre
    (dom/text "(dom/div
    (dom/text ")
    (dom/text var1)
    (dom/text ")
    (dom/div
      (let [a ")
    (dom/text "2")
    (dom/text "]
        (dom/text a ")
    (dom/text var3)
    (dom/text " ")
    (dom/text  var2)
    (dom/text "))))")))
