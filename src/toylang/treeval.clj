(ns toylang.treeval
  (:gen-class)
  (:require [toylang.ast :as ast]
            [toylang.defexpr :refer [variant]]
            [clojure.spec.alpha :as spec]))

(spec/def ::Env (spec/map-of symbol? #(instance? clojure.lang.IAtom %)))

(defn add-to-env [env name value]
  {:pre [(spec/assert ::Env env)
         (spec/assert ::ast/Name name)]
   :post [(spec/assert ::Env %)]}
  (assoc env (::ast/Name/sym name) (atom value)))

(defn add-to-env! [env name value]
  {:pre [(spec/assert #(instance? clojure.lang.ITransientCollection %)
                      env)
         (spec/assert ::ast/Name name)]
   :post [(spec/assert #(instance? clojure.lang.ITransientCollection %)                       
                       %)]}
  (assoc! env (::ast/Name/sym name) (atom value)))

(defn env-get-atom [env name]
  {:pre [(spec/assert ::Env env)
         (spec/assert ::ast/Name name)]}
  (if-let [value (get env (::ast/Name/sym name) nil)]
    value
    (undefined-variable name)))

(defn env-get [env name]
  {:pre [(spec/assert ::Env env)
         (spec/assert ::ast/Name name)]}
  (or @(env-get-atom env name)
      (throw (ex-info "Variable's value is nil"
                      {:variable name}))))

(defmulti treeval (fn [expr _] (variant expr)))

(defrecord UndefinedVariable [name])

(defn undefined-variable [name]
  {:pre [(spec/assert ::ast/Name name)]}
  (throw (ex-info "Undefined variable"
                  (UndefinedVariable. name))))

(defmethod treeval ::ast/Name [var env]
  {:pre [(spec/assert ::ast/Name var)
         (spec/assert ::Env env)]}
  (env-get env var))

(defmethod treeval ::ast/Lit [lit _]
  {:pre [(spec/assert ::ast/Lit lit)]}
  (::ast/Lit/value lit))

(defrecord Closure [function env])

(defmethod treeval ::ast/Fn [function env]
  {:pre [(spec/assert ::ast/Fn function)
         (spec/assert ::Env env)]}
  (Closure. function env))

(defmethod treeval ::ast/Let [lt env]
  {:pre [(spec/assert ::ast/Let lt)
         (spec/assert ::Env env)]}
  (let [value (treeval (::ast/Let/initform lt) env)
        inner-env (add-to-env env (::ast/Let/name lt) value)]
    (treeval (::ast/Let/body-expr lt) inner-env)))

(defmethod treeval ::ast/Begin [block env]
  {:pre [(spec/assert ::ast/Begin block)
         (spec/assert ::Env env)]}
  (treeval (::ast/Begin/first block) env)
  (treeval (::ast/Begin/second block) env))

(defmethod treeval ::ast/Call [call env]
  {:pre [(spec/assert ::ast/Call call)
         (spec/assert ::Env env)]}
  (let [fun (treeval (::ast/Call/operator call) env)]
    (assert (instance? Closure fun))
    (let [args (map (fn [arg] (treeval arg env))
                    (::ast/Call/operands call))
          arglist (::ast/Fn/arglist (:function fun))]
      (assert (= (count args) (count (::ast/Fn/arglist (:function fun)))))
      (let [binding-pairs (map list arglist args)
            inner-env (persistent! (reduce (fn [partial-env [name value]]
                                             (add-to-env! partial-env
                                                          name
                                                          value))
                                           (transient (:env fun))
                                           binding-pairs))]
        (treeval (::ast/Fn/body-expr (:function fun))
                 inner-env)))))

(defmethod treeval ::ast/LetRec [ltr env]
  {:pre [(spec/assert ::ast/LetRec ltr)
         (spec/assert ::Env env)]}
  (let [all-bindings (::ast/LetRec/bindings ltr)
        all-initforms (map second all-bindings)
        all-names (map first all-bindings)
        inner-env (persistent! (reduce (fn [partial-env next-name]
                                         (add-to-env! partial-env
                                                      next-name
                                                      nil))
                                       (transient env)
                                       all-names))
        all-closures (map (fn [initform]
                            (treeval initform inner-env))
                          all-initforms)]
    (doseq [name all-names
            closure all-closures]
      (reset! (env-get-atom inner-env name) closure))
    (treeval (::ast/LetRec/body-expr ltr) inner-env)))

(defmethod treeval ::ast/Cond [cnd env]
  {:pre [(spec/assert ::ast/Cond cnd)
         (spec/assert ::Env env)]}
  (let [matching-clause
        (some (fn [[condition _ :as clause]]
                (if (treeval condition env)
                  clause
                  false))
              (::ast/Cond/clauses cnd))]
    (if matching-clause
      (treeval (second matching-clause) env)
      false)))

(defmethod treeval ::ast/If [expr env]
  {:pre [(spec/assert ::ast/If expr)
         (spec/assert ::Env env)]}
  (treeval (if (treeval (::ast/If/condition expr) env)
             (::ast/If/then expr)
             (::ast/If/else expr))
           env))
