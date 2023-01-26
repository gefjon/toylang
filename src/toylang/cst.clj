(ns toylang.cst
  (:gen-class)
  (:require [toylang.defexpr :refer [defexpr]]))

(defexpr Name [sym])

(defexpr Lit
  [type
   value])

(defexpr Fn
  [name
   arglist
   body ;; a Seq of exprs
   ])

(defexpr Let
  [name
   initform])

(defexpr Call
  [operator
   operands])

(defexpr LetRec
  [bindings ;; a Seq of [NAME INITFORM] pairs
   ])

(defexpr Cond
  [clauses ;; a Seq of [TEST & BODY] Seqs
   ])

(defexpr If
  [condition
   then
   else])

(defmulti parse class)

(defmulti parse-list (fn [head & _] head))

(defmethod parse clojure.lang.PersistentList [lst]
  (apply parse-list lst))

(defmethod parse clojure.lang.Symbol [sym]
  (make-name sym))

(defmethod parse java.lang.Boolean [b]
  (make-lit ::Bool b))

(defmethod parse java.lang.Long [num]
  (make-lit ::Int num))

(defmethod parse java.lang.Integer [num]
  (make-lit ::Int (long num)))

(defmethod parse java.lang.String [str]
  (make-lit ::String str))

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
