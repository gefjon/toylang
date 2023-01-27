(ns toylang.ast
  (:gen-class)
  (:require [toylang.cst :as cst]
            [toylang.defexpr :refer [defexpr variant]]
            [clojure.spec.alpha :as spec]))

(spec/def ::Expr
  (spec/or ::Name ::Name
           ::Lit ::Lit
           ::Let ::Let
           ::Begin ::Begin
           ::Call ::Call
           ::LetRec ::LetRec
           ::Cond ::Cond
           ::If ::If))

(defexpr Name
  [[sym symbol?]])

(defexpr Lit
  [[value (spec/or ::Bool boolean? ::Int int? ::String string?)]])

(defexpr Fn
  [[name (spec/nilable ::Name)]
   [arglist (spec/coll-of ::Name)]
   [body-expr ::Expr]])

(defexpr Let
  [[name ::Name]
   [initform ::Expr]
   [body-expr ::Expr]])

(defexpr Begin
  [[first ::Expr]
   [second ::Expr]])

(defexpr Call
  [[operator ::Expr]
   [operands (spec/coll-of ::Expr)]])

(defexpr LetRec
  [[bindings (spec/coll-of (spec/tuple ::Name ::Fn))]
   [body-expr ::Expr]])

(defexpr Cond
  [[clauses (spec/coll-of (spec/tuple ::Expr ::Expr))]])

(defexpr If
  [[condition ::Expr]
   [then ::Expr]
   [else ::Expr]])

(defmulti transform-stmt (fn [expr _] (variant expr)))

(defmulti transform-expr variant)

(defn transform-continuation-varargs
  ([expr] expr)
  ([expr & continuation] (make-begin expr (apply transform-stmt continuation))))

(defn transform-continuation [expr cont]
  (apply transform-continuation-varargs expr cont))

(defn transform-body [body]
  (if (empty? body)
    (make-lit false)
    (let [[first & continuation] body]
      (transform-stmt first continuation))))

(defn transform-name [name]
  {:pre [(spec/assert ::cst/Name name)]
   :post [(spec/assert ::Name %)]}
  (make-name (::cst/Name/sym name)))

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
  (make-lit (::cst/Lit/value lit)))

(defmethod transform-stmt ::cst/Lit [lit continuation]
  {:pre [(spec/assert ::cst/Lit lit)
         (empty? continuation)]
   :post [(spec/assert ::Lit %)]}
  (transform-expr lit))

(defmethod transform-expr ::cst/Fn [fun]
  (make-fn (when-let [name (::cst/Fn/name fun)]
             (transform-name name))
           (map transform-name (::cst/Fn/arglist fun))
           (transform-body (::cst/Fn/body fun))))

(defmethod transform-stmt ::cst/Fn [fun continuation]
  (assert (and (not (empty? continuation))
               (::cst/Fn/name fun)))
  (make-letrec [[(transform-name (::cst/Fn/name fun)) (transform-expr fun)]]
               (transform-body continuation)))

(defmethod transform-stmt ::cst/Let [lt continuation]
  (assert (not (empty? continuation)))
  (make-let (transform-name (::cst/Let/name lt))
            (transform-expr (::cst/Let/initform lt))
            (transform-body continuation)))

(defmethod transform-expr ::cst/Call [call]
  (make-call (transform-expr (::cst/Call/operator call))
             (map transform-expr (::cst/Call/operands call))))

(defmethod transform-stmt ::cst/Call [call continuation]
  (transform-continuation (transform-expr call) continuation))

(defmethod transform-expr ::cst/Cond [cnd]
  (let [transform-clause (fn [clause]
                           (let [[test & body] clause]
                             [(transform-expr test)
                              (transform-body body)]))]
    (make-cond (map transform-clause (::cst/Cond/clauses cnd)))))

(defmethod transform-stmt ::cst/Cond [cnd continuation]
  (transform-continuation (transform-expr cnd) continuation))

(defmethod transform-expr ::cst/If [iff]
  (make-if (transform-expr (::cst/If/condition iff))
           (transform-expr (::cst/If/then iff))
           (transform-expr (::cst/If/else iff))))

(defmethod transform-stmt ::cst/If [iff continuation]
  (transform-continuation (transform-expr iff) continuation))

(defn transform-block [stmts]
  (let [[first & rest] stmts]
    (transform-stmt first rest)))
