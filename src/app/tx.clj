(ns app.db
  (:require
   [datascript.core :as d]))

(defn transact! [& args]
  (apply d/transact! args)
  nil)
