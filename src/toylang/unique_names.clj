(ns toylang.unique-names
  (:gen-class)
  (:require [toylang.ast :as ast]
            [toylang.defexpr :refer [spec-assert-then-true]]
            [clojure.spec.alpha :as spec])
  (:use [slingshot.slingshot :only [throw+ try+]]))

(spec/def ::Env (spec/map-of ::ast/Name ::ast/Name))

(defn- extend-env [env & pairs]
  {:pre [(spec/assert ::Env env)
         (spec-assert-then-true (spec/nilable (spec/coll-of (spec/tuple ::ast/Name ::ast/Name))) pairs)]
   :post [(spec/assert ::Env env)]}
  (persistent! (reduce (fn [partial-env [source-name rename]]
                         (assoc! partial-env source-name rename))
                       (transient env)
                       pairs)))

(defn- find-renaming-or [env name or-else]
  {:pre [(spec/assert ::Env env)
         (spec/assert ::ast/Name name)]
   :post [(spec/assert ::ast/Name %)]}
  (or (get env name)
      (or-else)))

(defn- find-renaming [env name]
  (find-renaming-or env name
                    (fn []
                      (throw+ {:type ::UnknownVariable
                               :var name
                               :env env}))))

(defmulti ^:private rename-in (fn [expr _] (:variant expr)))

(defn- rename [expr env]
  {:pre [(spec/assert ::ast/Expr expr)
         (spec/assert ::Env env)]
   :post [(spec/assert ::ast/Expr %)]}
  (rename-in expr env))

(defmethod rename-in ::ast/Name [name env]
  (find-renaming env name))

(defmethod rename-in ::ast/Lit [lit _] lit)

(defn- gen-name [name]
  {:pre [(spec/assert ::ast/Name name)]
   :post [(spec/assert ::ast/Name %)]}
  (ast/make-name (gensym (str (ast/sym name) "-"))))

(defn- pair-with-unique-name [name]
  {:pre [(spec/assert ::ast/Name name)]
   :post [(spec/assert (spec/tuple ::ast/Name ::ast/Name) %)]}
  [name (gen-name name)])

(defn- find-function-name
  "Find an appropriate renaming for the function `fun` within `env`.

  There are three cases to handle:
  - The function is bound by letrec and has a name, so its name is in the env already.
  - The function is not bound by letrec but has a name, so uniqueify it but do not add it to the env.
  - The function is anonymous, so leave its name as nil."
  [env fun]
  {:pre [(spec/assert ::Env env)
         (spec/assert ::ast/Fn fun)]
   :post [(spec-assert-then-true (spec/nilable ::ast/Name) %)]}
  (when-let [source-name (ast/name fun)]
    (find-renaming-or env
                      source-name
                      (fn [] (gen-name source-name)))))

(defmethod rename-in ::ast/Fn [fun env]
  (let [fname (find-function-name env fun)
        arg-renamings (map pair-with-unique-name (ast/arglist fun))
        new-arglist (map second arg-renamings)
        inner-env (apply extend-env env arg-renamings)
        body-expr (rename (ast/body-expr fun) inner-env)]
    (ast/make-fn fname new-arglist body-expr)))

(defmethod rename-in ::ast/Let [lt env]
  (let [source-name (ast/name lt)
        rename (gen-name source-name)
        initform (rename (ast/initform lt) env)
        inner-env (extend-env env [source-name rename])
        body-expr (rename (ast/body-expr lt) inner-env)]
    (ast/make-let rename initform body-expr)))

(defmethod rename-in ::ast/Begin [begin env]
  (ast/make-begin (rename (ast/first begin) env)
                  (rename (ast/second begin) env)))

(defmethod rename-in ::ast/Call [call env]
  (ast/make-call (rename (ast/operator call) env)
                 (map #(rename % env) (ast/operands call))))

(defmethod rename-in ::ast/LetRec [ltr env]
  (let [renamings (map (fn [[name _]]
                         (pair-with-unique-name name))
                       (ast/bindings ltr))
        inner-env (apply extend-env env renamings)
        initforms (map (fn [[_ initform]]
                         (rename initform inner-env))
                       (ast/bindings ltr))
        new-bindings (map (fn [[_ rename] initform]
                            [rename initform])
                          renamings initforms)
        body-expr (rename (ast/body-expr ltr) inner-env)]
    (ast/make-letrec new-bindings body-expr)))

(defmethod rename-in ::ast/If [iff env]
  (ast/make-if (rename (ast/condition iff) env)
               (rename (ast/then iff) env)
               (rename (ast/else iff) env)))

(defn rename-variables [program]
  {:pre [(spec/assert ::ast/Expr program)]
   :post [(spec/assert ::ast/Expr %)]}
  (rename program {}))
