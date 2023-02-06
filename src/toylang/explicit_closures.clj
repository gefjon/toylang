(ns toylang.explicit-closures
  (:gen-class)
  (:refer-clojure :exclude [name first second])
  (:require [toylang.defexpr :refer [defexpr define-language]]
            [toylang.ast :as ast]
            [clojure.spec.alpha :as spec]))

(define-language Expr)

(defexpr LocalName Expr [[sym symbol?]])

(defexpr ClosureName Expr [[sym symbol?]])

(defexpr Lit Expr
  [[value (spec/or ::Bool boolean? ::Int #(instance? java.lang.Long %) ::String string?)]])

(spec/def ::LocalEnv (spec/map-of ::ast/Name ::LocalName))

(spec/def ::ClosureEnv (spec/map-of ::ast/Name ::ClosureName))

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
   [operands (spec/coll-of ::Expr)]])

(defexpr LetRec Expr
  [[bindings (spec/coll-of (spec/tuple ::LocalName ::Fn))]
   [body-expr ::Expr]])

(defexpr If Expr
  [[condition ::Expr]
   [then ::Expr]
   [else ::Expr]])

(defn- find-name [name env]
  {:pre [(spec/assert ::ast/Name name)
         (spec/assert ::Env env)]
   :post [(spec/assert (spec/tuple (spec/or ::LocalName ::LocalName ::ClosureName ::ClosureName)
                                   ::ClosureEnv)
                       %)]}
  (if-let [local (get-in env [::LocalEnv name])]
    [local (::ClosureEnv env)]
    (let [closure (make-closurename (ast/sym name))]
      [closure (assoc (::ClosureEnv env)
                      name
                      closure)])))

(defn- add-locals [env & key-value-tuples]
  {:pre [(spec/assert ::Env env)
         (spec/assert (spec/coll-of (spec/tuple ::ast/Name ::LocalName)) key-value-tuples)]
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
  {:pre [(spec/assert ::ast/Expr expr)
         (spec/assert ::Env env)]
   :post [(spec/assert (spec/tuple ::Expr ::ClosureEnv) %)]}
  (transform-expr' expr env))

(defmethod transform-expr' ::ast/Lit [lit env]
  [(make-lit (ast/value lit)) (::ClosureEnv env)])

(defmethod transform-expr' ::ast/Name [name env]
  (find-name name env))

(defn- merge-closure-envs [& envs]
  {:pre [(spec/assert (spec/coll-of ::ClosureEnv) envs)]
   :post [(spec/assert ::ClosureEnv %)]}
  (into {} (apply concat envs)))

(defmethod transform-expr' ::ast/Fn [fun env]
  (let [arglist (map #(make-localname (ast/sym %)) (ast/arglist fun))
        inner-env (apply add-locals empty-env (map vector (ast/arglist fun) arglist))
        [body-expr inner-closure-env] (transform-expr (ast/body-expr fun) inner-env)
        [outer-closure-env fn-closure-env] (reduce (fn [[partial-outer-env partial-inner-env] [source-name inner-name]]
                                                     (let [[outer-name _] (find-name source-name env)]
                                                       [(cond
                                                          (spec/valid? ::ClosureName outer-name) (assoc partial-outer-env
                                                                                                        source-name
                                                                                                        outer-name)
                                                          (spec/valid? ::LocalName outer-name) partial-outer-env
                                                          :else (throw (ex-info "Expected either a LocalName or a ClosureName as outer-name in transform-expr' ::ast/Fn"
                                                                                {:found outer-name})))
                                                        (assoc partial-inner-env inner-name outer-name)]))
                                                   [{} {}]
                                                   inner-closure-env)]
    [(make-fn (when-let [fname (ast/name fun)]
                (make-localname (ast/sym fname)))
              arglist
              fn-closure-env
              body-expr)
     outer-closure-env]))

(defmethod transform-expr' ::ast/Let [lt env]
  (let [[initform initform-closure-vars] (transform-expr (ast/initform lt) env)
        name (ast/name lt)
        new-local (make-localname (ast/sym name))
        body-env (add-locals env [name new-local])
        [body-expr body-closure-vars] (transform-expr (ast/body-expr lt) body-env)
        full-closure-env (merge-closure-envs initform-closure-vars body-closure-vars (::ClosureEnv env))]
    [(make-let new-local initform body-expr)
     full-closure-env]))

(defmethod transform-expr' ::ast/Begin [begin env]
  (let [[first first-closure-vars] (transform-expr (ast/first begin) env)
        [second second-closure-vars] (transform-expr (ast/second begin) env)]
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

(defmethod transform-expr' ::ast/Call [call env]
  (let [[operands operand-closure-vars] (transform-call-operands (ast/operands call) env)
        [operator operator-closure-vars] (transform-expr (ast/operator call) env)]
    [(make-call operator operands)
     (merge-closure-envs operand-closure-vars operator-closure-vars)]))

(defmethod transform-expr' ::ast/LetRec [letrec env]
  (let [renames (map (fn [[name _]]
                        [name (make-localname (ast/sym name))])
                      (ast/bindings letrec))
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
                (ast/bindings letrec))
        
        [body-expr body-closure-vars] (transform-expr (ast/body-expr letrec) inner-env)]
    [(make-letrec bindings body-expr)
     (merge-closure-envs binding-closure-vars body-closure-vars)]))

(defmethod transform-expr' ::ast/If [iff env]
  (let [[condition condition-closure-env] (transform-expr (ast/condition iff) env)
        [then then-closure-env] (transform-expr (ast/then iff) env)
        [else else-closure-env] (transform-expr (ast/else iff) env)]
    [(make-if condition then else)
     (merge-closure-envs condition-closure-env then-closure-env else-closure-env)]))

(defn transform-program [program]
  {:pre [(spec/assert ::ast/Expr program)]
   :post [(spec/assert ::Expr %)]}
  (let [[new-program closure-env] (transform-expr program empty-env)]
    (if (empty? closure-env)
      new-program
      (throw (ex-info "Program references unknown variables"
                      {:source-program program
                       :transformed-program new-program
                       :unknown-variables closure-env})))))

