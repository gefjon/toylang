(ns toylang.core
  (:gen-class)
  (:require [toylang.cst :as cst]
            [toylang.ast :as ast]
            [toylang.treeval :as treeval]
            [clojure.spec.alpha :as spec]))

(defn compile-treeval [forms]
  (let [cst (cst/parse-block forms)
        ast (ast/transform-block cst)]
    (treeval/treeval ast {})))

(defn -main
  [& args]
  (if (empty? args)
    (println "Supply some forms in argv to be evalled!")
    (println (compile-treeval (map read-string args)))))
