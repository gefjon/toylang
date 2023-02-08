(ns toylang.explicit-letrec
  (:gen-class)
  (:refer-clojure :exclude [name first second])
  (:require [toylang.defexpr :refer [defexpr define-language]]
            [toylang.explicit-closures :as close]
            [clojure.spec.alpha :as spec]))

(define-language Expr)

(defexpr LocalName Expr [[sym symbol?]])

(defexpr ClosureName Expr [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(defexpr Uninit Expr
  [])

(def uninit (make-uninit))

(spec/def ::ClosureEnv (spec/map-of ::ClosureName (spec/or ::LocalName ::LocalName
                                                           ::ClosureName ::ClosureName
                                                           ::Uninit ::Uninit)))

(defexpr Fn Expr
  [[name (spec/nilable ::LocalName)]
   [arglist (spec/nilable (spec/coll-of ::LocalName))]
   [closure-env ::ClosureEnv]
   [body-expr ::Expr]])

(defexpr SetClosure Expr ; assign NAME to NEW-VALUE within the closure stored in TARGET. TARGET->NAME must exist and be Uninit. 
  [[target ::LocalName]
   [name ::ClosureName]
   [new-value ::LocalName]])

(defexpr Let Expr
  [[name ::LocalName]
   [initform ::Expr]
   [body-expr ::Expr]])

(defexpr Begin Expr
  [[first ::Expr]
   [second ::Expr]])

(defexpr Call Expr
  [[operator ::Expr]
   [operands (spec/nilable (spec/coll-of ::Expr))]])

(defexpr CallIf Expr
  [[condition ::LocalName]
   [then ::LocalName]
   [else ::LocalName]])

(defmulti ^:private transform-expr' :variant)

(defn transform-expr [expr]
  {:pre [(spec/assert ::close/Expr expr)]
   :post [(spec/assert ::Expr %)]}
  (transform-expr' expr))

;;; trivial transformations

(defmethod transform-expr' ::close/LocalName [ln]
  (make-localname (close/sym ln)))

(defmethod transform-expr' ::close/ClosureName [cn]
  (make-closurename (close/sym cn)))

(defmethod transform-expr' ::close/Lit [lit]
  (make-lit (close/value lit)))

(defmethod transform-expr' ::close/Let [lt]
  (make-let (transform-expr (close/name lt))
            (transform-expr (close/initform lt))
            (transform-expr (close/body-expr lt))))

(defmethod transform-expr' ::close/Begin [begin]
  (make-begin (transform-expr (close/first begin))
              (transform-expr (close/second begin))))

(defmethod transform-expr' ::close/Call [call]
  (make-call (transform-expr (close/operator call))
             (map transform-expr (close/operands call))))

(defmethod transform-expr' ::close/CallIf [iff]
  (make-callif (transform-expr (close/condition iff))
               (transform-expr (close/then iff))
               (transform-expr (close/else iff))))

;;; transforming functions and letrec of functions

(defn- transform-fn [fun find-outer-var]
  (make-fn (when-let [name (close/name fun)]
             (transform-expr name))
           (map transform-expr (close/arglist fun))
           (into {} (map (fn [[inner-name outer-name]]
                           [(transform-expr inner-name)
                            (find-outer-var outer-name)])
                         (close/closure-env fun)))
           (transform-expr (close/body-expr fun))))

(defmethod transform-expr' ::close/Fn [fun]
  (transform-fn fun transform-expr))

(defmethod transform-expr' ::close/LetRec [ltr]
  (let [body-expr (transform-expr (close/body-expr ltr))

        source-names (map clojure.core/first (close/bindings ltr))
        
        source-names-uninit (into {} (map (fn [name]
                                            [name uninit])
                                          source-names))
        find-outer-var (fn [name] (or (source-names-uninit name) (transform-expr name)))

        bindings (into [] (map (fn [[name fun]]
                                 [(transform-expr name) (transform-fn fun find-outer-var)])
                               (close/bindings ltr)))

        body-with-set (reduce (fn [partial-body [name fun]]
                                (reduce (fn [partial-body [inner-var outer-var]]
                                          (if (spec/valid? ::Uninit outer-var)
                                            (make-begin (make-setclosure name
                                                                         inner-var
                                                                         (make-localname (sym inner-var)))
                                                        partial-body)
                                            partial-body))
                                        partial-body
                                        (closure-env fun)))
                              body-expr
                              bindings)]
    (reduce (fn [partial-expr [name fun]]
              (make-let name fun partial-expr))
            body-with-set
            (rseq bindings))))
