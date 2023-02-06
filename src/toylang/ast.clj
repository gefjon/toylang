(ns toylang.ast
  (:gen-class)
  (:refer-clojure :exclude [name first second])
  (:require [toylang.cst :as cst]
            [toylang.defexpr :refer [defexpr define-language]]
            [clojure.spec.alpha :as spec]))

(define-language Expr)

(defexpr Name Expr
  [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(defexpr Fn Expr
  [[name (spec/nilable ::Name)]
   [arglist (spec/coll-of ::Name)]
   [body-expr ::Expr]])

(defexpr Let Expr
  [[name ::Name]
   [initform ::Expr]
   [body-expr ::Expr]])

(defexpr Begin Expr
  [[first ::Expr]
   [second ::Expr]])

(defexpr Call Expr
  [[operator ::Expr]
   [operands (spec/coll-of ::Expr)]])

(defexpr LetRec Expr
  [[bindings (spec/coll-of (spec/tuple ::Name ::Fn))]
   [body-expr ::Expr]])

(defexpr If Expr
  [[condition ::Expr]
   [then ::Expr]
   [else ::Expr]])

(defmulti ^:private transform-stmt (fn [expr _] (:variant expr)))

(defmulti ^:private transform-expr :variant)

(defn- transform-continuation-varargs
  ([expr] expr)
  ([expr & continuation] (make-begin expr (apply transform-stmt continuation))))

(defn- transform-continuation [expr cont]
  (apply transform-continuation-varargs expr cont))

(defn- transform-body [body]
  (if (empty? body)
    (make-lit false)
    (let [[first & continuation] body]
      (transform-stmt first continuation))))

(defn- transform-name [name]
  {:pre [(spec/assert ::cst/Name name)]
   :post [(spec/assert ::Name %)]}
  (make-name (cst/sym name)))

(defmethod transform-expr ::cst/Name [name]
  {:pre [(spec/assert ::cst/Name name)]
   :post [(spec/assert ::Name %)]}
  (transform-name name))

(defmethod transform-stmt ::cst/Name [name continuation]
  {:pre [(spec/assert ::cst/Name name)
         (empty? continuation)]
   :post [(spec/assert ::Name %)]}
  (transform-expr name))

(defmethod transform-expr ::cst/Lit [lit]
  {:pre [(spec/assert ::cst/Lit lit)]
   :post [(spec/assert ::Lit %)]}
  (make-lit (cst/value lit)))

(defmethod transform-stmt ::cst/Lit [lit continuation]
  {:pre [(spec/assert ::cst/Lit lit)
         (empty? continuation)]
   :post [(spec/assert ::Lit %)]}
  (transform-expr lit))

(defmethod transform-expr ::cst/Fn [fun]
  (make-fn (when-let [name (cst/name fun)]
             (transform-name name))
           (map transform-name (cst/arglist fun))
           (transform-body (cst/body fun))))

(defmethod transform-stmt ::cst/Fn [fun continuation]
  (assert (and (not (empty? continuation))
               (cst/name fun)))
  (make-letrec [[(transform-name (cst/name fun)) (transform-expr fun)]]
               (transform-body continuation)))

(defmethod transform-stmt ::cst/Let [lt continuation]
  (assert (not (empty? continuation)))
  (make-let (transform-name (cst/name lt))
            (transform-expr (cst/initform lt))
            (transform-body continuation)))

(defmethod transform-expr ::cst/Call [call]
  (make-call (transform-expr (cst/operator call))
             (map transform-expr (cst/operands call))))

(defmethod transform-stmt ::cst/Call [call continuation]
  (transform-continuation (transform-expr call) continuation))

(defmethod transform-stmt ::cst/LetRec [ltr continuation]
  (make-letrec (map (fn [[name initform]]
                      (spec/assert ::cst/Fn initform)
                      [(transform-name name)
                       (transform-expr initform)])
                    (cst/bindings ltr))
               (transform-body continuation)))

(defmethod transform-expr ::cst/Cond [cnd]
  (reduce (fn [else-case [test & body]]
            (make-if (transform-expr test)
                     (transform-body body)
                     else-case))
          (make-lit false)
          (rseq (cst/clauses cnd))))

(defmethod transform-stmt ::cst/Cond [cnd continuation]
  (transform-continuation (transform-expr cnd) continuation))

(defmethod transform-expr ::cst/If [iff]
  (make-if (transform-expr (cst/condition iff))
           (transform-expr (cst/then iff))
           (transform-expr (cst/else iff))))

(defmethod transform-stmt ::cst/If [iff continuation]
  (transform-continuation (transform-expr iff) continuation))

(defn transform-block [stmts]
  (let [[first & rest] stmts]
    (transform-stmt first rest)))
