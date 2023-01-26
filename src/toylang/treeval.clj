(ns toylang.treeval
  (:gen-class)
  (:require [toylang.ast :as ast]))

(defmulti treeval (fn [expr _] (:variant expr)))

(defrecord UndefinedVariable [name])

(defn undefined-variable [name]
  (throw (ex-info "Undefined variable"
                  (UndefinedVariable. name))))

(defmethod treeval ::ast/Name [var env]
  (let [value (get env (:name var) ::not-found)]
    (if (= value ::not-found)
      (undefined-variable var)
      @value)))

(defmethod treeval ::ast/Lit [lit _]
  (:value lit))

(defrecord Closure [function env])

(defmethod treeval ::ast/Fn [function env]
  (Closure. function env))

(defmethod treeval ::ast/Let [lt env]
  (let [value (treeval (:initform lt) env)
        inner-env (assoc env (:name lt) (atom value))]
    (treeval (:body-expr lt) inner-env)))

(defmethod treeval ::ast/Begin [block env]
  (treeval (:first block) env)
  (treeval (:second block) env))

(defmethod treeval ::ast/Call [call env]
  (let [fun (treeval (:operator call) env)]
    (assert (instance? Closure fun))
    (let [args (map (fn [arg] (treeval arg env))
                    (:operands call))
          arglist (:arglist (:function fun))]
      (assert (= (count args) (count (:arglist (:function fun)))))
      (let [binding-pairs (map list arglist args)
            inner-env (persistent! (reduce (fn [partial-env [name value]]
                                             (assoc! partial-env
                                                     (:name name)
                                                     (atom value)))
                                           (transient (:env fun))
                                           binding-pairs))]
        (treeval (:body-expr (:function fun))
                 inner-env)))))

(defmethod treeval ::ast/LetRec [ltr env]
  (assert (every? (fn [[_ expr]] (= (:variant expr) ::ast/Fn)) (:bindings ltr)))
  (let [all-bindings (:bindings ltr)
        all-initforms (map second all-bindings)
        all-names (map first all-bindings)
        inner-env (persistent! (reduce (fn [partial-env next-name]
                                         (assoc! partial-env
                                                 (:name next-name)
                                                 (atom nil)))
                                       (transient env)
                                       all-names))
        all-closures (map (fn [initform]
                            (treeval initform inner-env))
                          all-initforms)]
    (doseq [name all-names
            closure all-closures]
      (reset! (get inner-env (:name name)) closure))
    (treeval (:body-expr ltr) inner-env)))

(defmethod treeval ::ast/Cond [cnd env]
  (let [matching-clause
        (some (fn [[condition _ :as clause]]
                (if (treeval condition env)
                  clause
                  false))
              (:bindings cnd))]
    (if matching-clause
      (treeval (second matching-clause) env)
      false)))

(defmethod treeval ::ast/If [expr env]
  (treeval (if (treeval (:condition expr) env)
             (:then expr)
             (:else expr))
           env))
