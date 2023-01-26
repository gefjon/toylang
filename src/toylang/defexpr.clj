(ns toylang.defexpr
  (:gen-class)
  (:require [clojure.string :as str]))

(defmacro defexpr [name [& fields]]
  `(do (defrecord ~name [~'variant ~@fields])
       (defn ~(symbol (str "make-" (str/lower-case name))) [~@fields]
         (~(symbol (str name ".")) ~(keyword (str (ns-name *ns*)) (str name)) ~@fields))))
