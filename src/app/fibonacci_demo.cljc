(ns app.fibonacci-demo
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            clojure.math))

;; Server state
#?(:clj
   (do
     (defonce !index (atom 0))
     (defonce !mode (atom :fib))))

;; Reactive signals derived from atoms
(e/def index (e/server (e/watch !index)))
(e/def mode (e/server (e/watch !mode)))
(e/def opposing-mode (flip-mode mode))

(defn flip-mode [mode]
  (case mode
    :fib       :factorial
    :factorial :fib))
(defn mode-to-mode-name [mode]
  (case mode
    :fib       "Fibonacci"
    :factorial "Factorial"))
(e/def mode-name (mode-to-mode-name mode))
(e/def opposing-mode-name
  (mode-to-mode-name opposing-mode))

;; Client side fibonacci sequence generator
(defn fib-seq [n]
  (loop [n n a 0 b 1 acc []]
    (if (zero? n)
      acc
      (recur (dec n) b (+ a b) (conj acc a)))))

(defn factorial-seq [n]
  (loop [n n fact 1 acc []]
    (if (zero? n)
      acc
      (recur (dec n) (* fact (inc (count acc))) (conj acc fact)))))


(e/defn Button [func text]
  (ui/button func
    (dom/props {:class "btn hover:bg-base-100 block mt-2"})
    (dom/text text)))

(e/defn FibonacciApp []
  (e/client
   (dom/div
     (dom/props {:class "m-10"})
     ;; Display the fibonacci sequence
     (dom/div
       (dom/text
        mode-name
        " sequence: "
        ((case mode
           :fib       fib-seq
           :factorial factorial-seq) index)))
     (Button. (e/fn []
                (e/server (swap! !index inc)))
              (str "Next " mode-name " number"))
     (dom/div
       (dom/props {:class "flex"})
       (let [!cut-idx (atom 0), cut-idx (e/watch !cut-idx)]
         (ui/button (e/fn []
                      (e/server (reset! !index cut-idx)))
           (dom/props {:class "btn hover:bg-base-100 block mt-2"})
           (dom/dl
            (dom/props {:class "ml-2 mt-1 flex"})
            (dom/dt (dom/text "Cut-off Index"))
            (dom/dd
             (dom/props {:class "ml-2"})
             (ui/long
              (clojure.math/round cut-idx)
              (e/fn [v]
                (reset! !cut-idx v))))))))
     (Button. (e/fn []
                (e/server
                 (if (or (= @!mode :fib)
                         (= @!mode :factorial))
                   (swap! !mode flip-mode)
                   (reset! mode :fib))))
              (str "To " opposing-mode-name)))))
