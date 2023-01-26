(ns toylang.ast
  (:gen-class)
  (:require [toylang.cst :as cst]
            [toylang.defexpr :refer [defexpr]]))

(defexpr Name
  [sym])

(defexpr Lit
  [type
   value])

(defexpr Fn
    [name
     arglist
     body-expr])

(defexpr Let
    [name
     initform
     body-expr])

(defexpr Begin
  [first second])

(defexpr Call
    [operator
     operands])

(defexpr LetRec
    [bindings
     body-expr])

(defexpr Cond
    [clauses])

(defexpr If
    [condition
     then
     else])

(defmulti transform-stmt (fn [expr _] (:variant expr)))

(defmulti transform-expr :variant)

(defmulti transform-name :variant)

(defn transform-continuation-varargs
  ([expr] expr)
  ([expr & continuation] (make-begin expr (apply transform-stmt continuation))))

(defn transform-continuation [expr cont]
  (apply transform-continuation-varargs expr cont))

(defn transform-body [body]
  (let [[first & continuation] body]
    (transform-stmt first continuation)))

(defmethod transform-name ::cst/Name [name]
  (make-name (:sym name)))

(defmethod transform-expr ::cst/Name [name]
  (transform-name name))

(defmethod transform-stmt ::cst/Name [name continuation]
  (assert (empty? continuation))
  (transform-expr name))

(defmethod transform-expr ::cst/Lit [lit]
  (make-lit (case (:type lit)
              ::cst/Int ::Int
              ::cst/String ::String
              ::cst/Bool ::Bool)
            (:value lit)))

(defmethod transform-stmt ::cst/Lit [lit continuation]
  (assert (empty? continuation))
  (transform-expr lit))

(defmethod transform-expr ::cst/Fn [fun]
  (make-fn (transform-name (:name fun))
           (map transform-name (:arglist fun))
           (transform-body (:body fun))))

(defmethod transform-stmt ::cst/Fn [fun continuation]
  (assert (and (not (empty? continuation))
               (:name fun)))
  (make-letrec [[(transform-name (:name fun)) (transform-expr fun)]]
               (transform-body continuation)))

(defmethod transform-stmt ::cst/Let [lt continuation]
  (assert (not (empty? continuation)))
  (make-let (transform-name (:name lt))
            (transform-expr (:initform lt))
            (transform-body continuation)))

(defmethod transform-expr ::cst/Call [call]
  (make-call (transform-expr (:operator call))
             (map transform-expr (:operands call))))

(defmethod transform-stmt ::cst/Call [call continuation]
  (transform-continuation (transform-expr call) continuation))

(defmethod transform-expr ::cst/Cond [cnd]
  (let [transform-clause (fn [clause]
                           (let [[test & body] clause]
                             (cons (transform-expr test)
                                   (transform-body body))))]
    (make-cond (map transform-clause (:clauses cnd)))))

(defmethod transform-stmt ::cst/Cond [cnd continuation]
  (transform-continuation (transform-expr cnd) continuation))

(defmethod transform-expr ::cst/If [iff]
  (make-if (transform-expr (:condition iff))
           (transform-expr (:then iff))
           (transform-expr (:else iff))))

(defmethod transform-stmt ::cst/If [iff continuation]
  (transform-continuation (transform-expr iff) continuation))

(defn transform-block [stmts]
  (let [[first & rest] stmts]
    (transform-stmt first rest)))
