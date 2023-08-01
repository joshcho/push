(ns app.tx
  (:require
   [datascript.core :as d]))

(defn transact! [& args]
  (apply d/transact! args)
  nil)
