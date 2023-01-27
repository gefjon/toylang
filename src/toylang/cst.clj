(ns toylang.cst
  (:gen-class)
  (:require [toylang.defexpr :refer [defexpr]]
            [clojure.spec.alpha :as spec]))

(spec/def ::Expr
  (spec/or ::Name ::Name
           ::Lit ::Lit
           ::Fn ::Fn
           ::Let ::Let
           ::Call ::Call
           ::LetRec ::LetRec
           ::Cond ::Cond
           ::If ::If))

(defexpr Name [[sym symbol?]])

(defexpr Lit
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(defexpr Fn
  [[name (spec/nilable ::Name)]
   [arglist (spec/coll-of ::Name)]
   [body (spec/coll-of ::Expr)] ;; a Seq of exprs
   ])

(defexpr Let
  [[name ::Name]
   [initform ::Expr]])

(defexpr Call
  [[operator ::Expr]
   [operands (spec/coll-of ::Expr)]])

(defexpr LetRec
  [[bindings (spec/coll-of (spec/tuple ::Name ::Expr))]])

(defexpr Cond
  [[clauses (spec/and (spec/coll-of ::Expr) #(>= (count %) 1))]])

(defexpr If
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
            (parse body)))
  ([_ name-or-arglist arglist-or-first-body & body]
   (if (or (symbol? name-or-arglist)
           (string? name-or-arglist))
     (make-fn (parse name-or-arglist)
              (map parse arglist-or-first-body)
              (map parse body))
     (make-fn nil
              (map parse name-or-arglist)
              (map parse (cons arglist-or-first-body
                               body))))))

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
