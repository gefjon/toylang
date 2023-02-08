(ns toylang.cst
  (:gen-class)
  (:refer-clojure :exclude [name])
  (:require [toylang.defexpr :refer [defexpr define-language]]
            [clojure.spec.alpha :as spec]))

(define-language Expr)

(defexpr Name Expr [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(defexpr Fn Expr
  [[name (spec/nilable ::Name)]
   [arglist (spec/nilable (spec/coll-of ::Name))]
   [body (spec/coll-of ::Expr)] ;; a Seq of exprs
   ])

(defexpr Let Expr
  [[name ::Name]
   [initform ::Expr]])

(defexpr Call Expr
  [[operator ::Expr]
   [operands (spec/nilable (spec/coll-of ::Expr))]])

(defexpr LetRec Expr
  [[bindings (spec/coll-of (spec/tuple ::Name ::Expr))]])

(defexpr Cond Expr
  [[clauses (spec/and (spec/coll-of ::Expr) #(>= (count %) 1))]])

(defexpr If Expr
  [[condition ::Expr]
   [then ::Expr]
   [else ::Expr]])

(defmulti parse class)

(defmulti parse-list (fn [head & _] head))

(defmethod parse clojure.lang.PersistentList [lst]
  (apply parse-list lst))

(defmethod parse clojure.lang.Symbol [sym]
  (make-name sym))

(defmethod parse java.lang.Boolean [b]
  (make-lit b))

(defmethod parse java.lang.Long [num]
  (make-lit num))

(defmethod parse java.lang.Integer [num]
  (make-lit (long num)))

(defmethod parse java.lang.String [str]
  (make-lit str))

(defmethod parse-list 'fn
  ([_ arglist body]
   (make-fn nil
            (map parse arglist)
            [(parse body)]))
  ([_ name-or-arglist arglist-or-first-body & body]
   (if (or (symbol? name-or-arglist)
           (string? name-or-arglist))
     (make-fn (parse name-or-arglist)
              (into [] (map parse arglist-or-first-body))
              (into [] (map parse body)))
     (make-fn nil
              (into [] (map parse name-or-arglist))
              (into [] (map parse (cons arglist-or-first-body
                                        body)))))))

(defmethod parse-list 'let [_ name initform]
  (make-let (parse name)
            (parse initform)))

(defmethod parse-list 'letrec [_ & bindings]
  (make-letrec (map (fn [[name initform]]
                      [(parse name) (parse initform)])
                    bindings)))

(defmethod parse-list 'cond [_ & clauses]
  (make-cond (map (fn [[condition & body]]
                    (cons (parse condition) (map parse body)))
                  clauses)))

(defmethod parse-list 'if [_ test then else]
  (make-if (parse test)
           (parse then)
           (parse else)))

(defmethod parse-list :default [operator & operands]
  (make-call (parse operator)
             (map parse operands)))

(defn parse-block [block]
  (map parse block))
