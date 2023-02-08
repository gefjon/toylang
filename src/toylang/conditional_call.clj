(ns toylang.conditional-call
  (:gen-class)
  (:refer-clojure :exclude [name first second])
  (:require [toylang.ast :as ast]
            [toylang.defexpr :refer [defexpr define-language]]
            [clojure.spec.alpha :as spec]))

(define-language Expr)

(defexpr Name Expr [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(defexpr Fn Expr
  [[name (spec/nilable ::Name)]
   [arglist (spec/nilable (spec/coll-of ::Name))]
   [body-expr ::Expr]])

(defexpr Let Expr
  [[name ::Name]
   [initform ::Expr]
   [body-expr ::Expr]])

(defexpr LetRec Expr
  [[bindings (spec/coll-of (spec/tuple ::Name ::Fn))]
   [body-expr ::Expr]])

(defexpr Begin Expr
  [[first ::Expr]
   [second ::Expr]])

(defexpr Call Expr
  [[operator ::Expr]
   [operands (spec/nilable (spec/coll-of ::Expr))]])

(defexpr CallIf Expr
  [[condition ::Name]
   [then ::Name]
   [else ::Name]])

(defmulti ^:private transform-expr' :variant)

(defn transform-expr [expr]
  {:pre [(spec/assert ::ast/Expr expr)]
   :post [(spec/assert ::Expr %)]}
  (transform-expr' expr))

;;; trivial transformations

(defmethod transform-expr' ::ast/Name [name]
  (make-name (ast/sym name)))

(defmethod transform-expr' ::ast/Lit [lit]
  (make-lit (ast/value lit)))

(defmethod transform-expr' ::ast/Fn [fun]
  (make-fn (when-let [name (ast/name fun)]
             (transform-expr name))
           (map transform-expr (ast/arglist fun))
           (transform-expr (ast/body-expr fun))))

(defmethod transform-expr' ::ast/Let [lt]
  (make-let (transform-expr (ast/name lt))
            (transform-expr (ast/initform lt))
            (transform-expr (ast/body-expr lt))))

(defmethod transform-expr' ::ast/Begin [begin]
  (make-begin (transform-expr (ast/first begin))
              (transform-expr (ast/second begin))))

(defmethod transform-expr' ::ast/Call [call]
  (make-call (transform-expr (ast/operator call))
             (map transform-expr (ast/operands call))))

(defmethod transform-expr' ::ast/LetRec [ltr]
  (make-letrec (map (fn [[name initform]]
                      [(transform-expr name)
                       (transform-expr initform)])
                    (ast/bindings ltr))
               (transform-expr (ast/body-expr ltr))))

;;; transforming if to a conditional call

(defn- gen-name [prefix]
  {:post [(spec/assert ::Name %)]}
  (make-name (gensym prefix)))

(defmethod transform-expr' ::ast/If [iff]
  (let [condition-binding (gen-name "if-condition-")
        then-binding (gen-name "if-then-")
        else-binding (gen-name "if-else-")
        then-fn (make-fn nil [] (transform-expr (ast/then iff)))
        else-fn (make-fn nil [] (transform-expr (ast/else iff)))]
    (make-let condition-binding (transform-expr (ast/condition iff))
              (make-let then-binding then-fn
                        (make-let else-binding else-fn
                                  (make-callif condition-binding then-binding else-binding))))))

;;; entry point

(defn transform-program [prog]
  {:pre [(spec/assert ::ast/Expr prog)]
   :post [(spec/assert ::Expr %)]}
  (transform-expr prog))
