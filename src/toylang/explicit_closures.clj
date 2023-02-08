(ns toylang.explicit-closures
  (:gen-class)
  (:refer-clojure :exclude [name first second])
  (:require [toylang.defexpr :refer [defexpr define-language spec-assert-then-true]]
            [toylang.conditional-call :as cond]
            [clojure.spec.alpha :as spec])
  (:use [slingshot.slingshot :only [throw+ try+]]))

(define-language Expr)

(defexpr LocalName Expr [[sym symbol?]])

(defexpr ClosureName Expr [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(spec/def ::LocalEnv (spec/map-of ::cond/Name ::LocalName))

(spec/def ::ClosureEnv (spec/map-of ::cond/Name ::ClosureName))

(spec/def ::Env (spec/keys :req [::LocalEnv ::ClosureEnv]))

(defexpr Fn Expr
  [[name (spec/nilable ::LocalName)]
   [arglist (spec/coll-of ::LocalName)]
   [closure-env (spec/map-of ::ClosureName ::Expr)]
   [body-expr ::Expr]])

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

(defexpr LetRec Expr
  [[bindings (spec/coll-of (spec/tuple ::LocalName ::Fn))]
   [body-expr ::Expr]])

(defexpr CallIf Expr
  [[condition ::LocalName]
   [then ::LocalName]
   [else ::LocalName]])

(defn- find-name [name env]
  {:pre [(spec/assert ::cond/Name name)
         (spec/assert ::Env env)]
   :post [(spec/assert (spec/tuple (spec/or ::LocalName ::LocalName ::ClosureName ::ClosureName)
                                   ::ClosureEnv)
                       %)]}
  (if-let [local (get-in env [::LocalEnv name])]
    [local (::ClosureEnv env)]
    (let [closure (make-closurename (cond/sym name))]
      [closure (assoc (::ClosureEnv env)
                      name
                      closure)])))

(defn- add-locals [env & key-value-tuples]
  {:pre [(spec/assert ::Env env)
         (spec-assert-then-true (spec/nilable (spec/coll-of (spec/tuple ::cond/Name ::LocalName))) key-value-tuples)]
   :post [(spec/assert ::Env %)]}
  (update env
          ::LocalEnv
          (fn [local-env]
            (persistent! (reduce (fn [partial-env [name local]]
                                   (assoc! partial-env name local))
                                 (transient local-env)
                                 key-value-tuples)))))

(def ^:private empty-env {::LocalEnv {}
                          ::ClosureEnv {}})

(defmulti ^:private transform-expr' (fn [expr _] (:variant expr)))

(defn- transform-expr [expr env]
  {:pre [(spec/assert ::cond/Expr expr)
         (spec/assert ::Env env)]
   :post [(spec/assert (spec/tuple ::Expr ::ClosureEnv) %)]}
  (transform-expr' expr env))

(defmethod transform-expr' ::cond/Lit [lit env]
  [(make-lit (cond/value lit)) (::ClosureEnv env)])

(defmethod transform-expr' ::cond/Name [name env]
  (find-name name env))

(defn- merge-closure-envs [& envs]
  {:pre [(spec/assert (spec/coll-of ::ClosureEnv) envs)]
   :post [(spec/assert ::ClosureEnv %)]}
  (into {} (apply concat envs)))

(defmethod transform-expr' ::cond/Fn [fun env]
  (let [arglist (map #(make-localname (cond/sym %)) (cond/arglist fun))
        inner-env (apply add-locals empty-env (map vector (cond/arglist fun) arglist))
        [body-expr inner-closure-env] (transform-expr (cond/body-expr fun) inner-env)
        [outer-closure-env fn-closure-env] (reduce (fn [[partial-outer-env partial-inner-env] [source-name inner-name]]
                                                     (let [[outer-name _] (find-name source-name env)]
                                                       [(cond
                                                          (spec/valid? ::ClosureName outer-name) (assoc partial-outer-env
                                                                                                        source-name
                                                                                                        outer-name)
                                                          (spec/valid? ::LocalName outer-name) partial-outer-env
                                                          :else (throw+ {:type ::TypeError
                                                                         :wanted '(or ::LocalName ::ClosureName)
                                                                         :found outer-name}))
                                                        (assoc partial-inner-env inner-name outer-name)]))
                                                   [{} {}]
                                                   inner-closure-env)]
    [(make-fn (when-let [fname (cond/name fun)]
                (make-localname (cond/sym fname)))
              arglist
              fn-closure-env
              body-expr)
     outer-closure-env]))

(defmethod transform-expr' ::cond/Let [lt env]
  (let [[initform initform-closure-vars] (transform-expr (cond/initform lt) env)
        name (cond/name lt)
        new-local (make-localname (cond/sym name))
        body-env (add-locals env [name new-local])
        [body-expr body-closure-vars] (transform-expr (cond/body-expr lt) body-env)
        full-closure-env (merge-closure-envs initform-closure-vars body-closure-vars (::ClosureEnv env))]
    [(make-let new-local initform body-expr)
     full-closure-env]))

(defmethod transform-expr' ::cond/Begin [begin env]
  (let [[first first-closure-vars] (transform-expr (cond/first begin) env)
        [second second-closure-vars] (transform-expr (cond/second begin) env)]
    [(make-begin first second)
     (merge-closure-envs first-closure-vars second-closure-vars)]))

(defn- transform-call-operands [operands env]
  (reduce (fn [[partial-operands partial-closure-vars] next-operand]
            (let [[transformed-operand next-closure-vars]
                  (transform-expr next-operand env)]
              [(conj partial-operands transformed-operand)
               (merge-closure-envs partial-closure-vars next-closure-vars)]))
          [[] {}]
          operands))

(defmethod transform-expr' ::cond/Call [call env]
  (let [[operands operand-closure-vars] (transform-call-operands (cond/operands call) env)
        [operator operator-closure-vars] (transform-expr (cond/operator call) env)]
    [(make-call operator operands)
     (merge-closure-envs operand-closure-vars operator-closure-vars)]))

(defmethod transform-expr' ::cond/LetRec [letrec env]
  (let [renames (map (fn [[name _]]
                        [name (make-localname (cond/sym name))])
                      (cond/bindings letrec))
        inner-env (spec/assert ::Env (apply add-locals env renames))
        
        [bindings binding-closure-vars]
        (reduce (fn [[partial-bindings partial-closure-env] [name initform]]
                  (let [pair (find-name name inner-env)]
                    (spec/assert (spec/tuple ::LocalName ::ClosureEnv) pair)
                    (let [localname (nth pair 0) ; not `first` because it's shadowed
                          [new-initform new-closure-env] (transform-expr initform inner-env)]
                      [(conj partial-bindings [localname new-initform])
                       (merge-closure-envs partial-closure-env new-closure-env)])))
                [[] {}]
                (cond/bindings letrec))
        
        [body-expr body-closure-vars] (transform-expr (cond/body-expr letrec) inner-env)]
    [(make-letrec bindings body-expr)
     (merge-closure-envs binding-closure-vars body-closure-vars)]))

(defmethod transform-expr' ::cond/CallIf [iff env]
  (let [[condition _] (find-name (cond/condition iff) env)
        [then _] (find-name (cond/then iff) env)
        [else _] (find-name (cond/else iff) env)]
    (spec/assert ::LocalName condition)
    (spec/assert ::LocalName then)
    (spec/assert ::LocalName else)
    [(make-callif condition then else)
     (::ClosureEnv env)]))

(defn transform-program [program]
  {:pre [(spec/assert ::cond/Expr program)]
   :post [(spec/assert ::Expr %)]}
  (let [[new-program closure-env] (transform-expr program empty-env)]
    (if (empty? closure-env)
      new-program
      (throw+ {:type ::UnknownVariables
               :source-program program
               :transformed-program new-program
               :unknown-variables closure-env}))))

