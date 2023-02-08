(ns toylang.core
  (:gen-class)
  (:refer-clojure :exclude [compile])
  (:require [toylang.cst :as cst]
            [toylang.ast :as ast]
            [toylang.conditional-call :as cond]
            [toylang.explicit-closures :as close]
            [toylang.explicit-letrec :as ltr]
            [toylang.treeval :as treeval]
            [toylang.threeaddr :as threeaddr]
            [toylang.threeval :as threeval]
            [clojure.spec.alpha :as spec]))

(defn compile [forms]
  (let [cst (cst/parse-block forms)
        ast (ast/transform-block cst)
        conditional-call (cond/transform-program ast)
        closed (close/transform-program conditional-call)
        letreced (ltr/transform-expr closed)
        threeaddr (threeaddr/transform-program letreced)]
    threeaddr))

(defn compile-treeval [forms]
  (let [cst (cst/parse-block forms)
        ast (ast/transform-block cst)]
    (treeval/treeval ast {})))

(defn compile-threeval [forms]
  (let [compiled (compile forms)]
    (threeval/eval compiled)))

(defn -main
  [& args]
  (spec/check-asserts true)
  (if (empty? args)
    (println "Supply some forms in argv to be evalled!")
    (println (compile-threeval (map read-string args)))))
