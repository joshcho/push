(ns app.tx
  (:require
   [datalevin.core :as d]
   ;; [datascript.core :as d]
   ))

(defn transact!
  "Equivalent to d/transact! but for use in hyperfiddle.electric/server blocks."
  ([conn tx-data]
   (d/transact! conn tx-data)
   nil)
  ([conn tx-data tx-meta]
   (d/transact! conn tx-data tx-meta)
   nil))
