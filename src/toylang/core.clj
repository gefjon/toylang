(ns toylang.core
  (:gen-class)
  (:refer-clojure :exclude [compile])
  (:require [toylang.cst :as cst]
            [toylang.ast :as ast]
            [toylang.explicit-closures :as close]
            [toylang.explicit-letrec :as ltr]
            [toylang.treeval :as treeval]
            [toylang.threeaddr :as threeaddr]
            [clojure.spec.alpha :as spec]))

(defn compile [forms]
  (let [cst (cst/parse-block forms)
        ast (ast/transform-block cst)
        closed (close/transform-program ast)
        letreced (ltr/transform-expr closed)
        threeaddr (threeaddr/transform-program letreced)]
    threeaddr))

(defn compile-treeval [forms]
  (let [cst (cst/parse-block forms)
        ast (ast/transform-block cst)]
    (treeval/treeval ast {})))

(defn -main
  [& args]
  (spec/check-asserts true)
  (if (empty? args)
    (println "Supply some forms in argv to be evalled!")
    (println (compile (map read-string args)))))
