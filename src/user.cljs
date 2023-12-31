(ns ^:dev/always user ; Electric currently needs to rebuild everything when any file changes. Will fix
  (:require
   ;; app.todo-list
   ;; app.fibonacci-demo
   app.push
   ;; app.scenes
   ;; app.recursion
   ;; app.experiment
   ;; app.demo-todos-advanced
   ;; app.reactive-render
   ;; app.temperature-converter
   hyperfiddle.electric
   [hyperfiddle.electric-dom2 :as dom]))

#_(def electric-main
    (hyperfiddle.electric/boot ; Electric macroexpansion - Clojure to signals compiler
     (binding [hyperfiddle.electric-dom2/node js/document.body]
       (app.todo-list/Todo-list.))))
(def electric-main
  (hyperfiddle.electric/boot ; Electric macroexpansion - Clojure to signals compiler
   (binding [hyperfiddle.electric-dom2/node js/document.body]
     ;; (app.fibonacci-demo/FibonacciApp.)
     ;; (app.temperature-converter/TemperatureConverter.)
     ;; (app.scenes/ScenesApp.)
     (app.push/PushApp.)
     ;; (app.reactive-render/ReactiveRenderApp.)
     ;; (app.experiment/ExperimentApp.)
     ;; (app.experiment/App.)

     ;; (app.demo-todos-advanced/AdvancedTodoList.)
     )))

(defonce reactor nil)

(defn ^:dev/after-load ^:export start! []
  (assert (nil? reactor) "reactor already running")
  (set! reactor (electric-main
                  #(js/console.log "Reactor success:" %)
                  #(js/console.error "Reactor failure:" %))))

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))
