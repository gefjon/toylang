(ns toylang.threeaddr
  (:gen-class)
  (:refer-clojure :exclude [num into cond name])
  (:require [toylang.explicit-letrec :as ltr]
            [toylang.defexpr :refer [defexpr define-language]]
            [clojure.spec.alpha :as spec]))

(define-language Value)

(defexpr Lit Value
  [[value (spec/or ::Bool boolean?
                   ::Int #(instance? java.lang.Long %)
                   ::String string?)]])

(defexpr Uninit Value [])

(def uninit (make-uninit))

(defexpr LocalVariable Value
  [[sym symbol?]])

(defexpr ClosureVariable Value
  [[sym symbol?]])

(define-language CodeName)

(defexpr FunctionName CodeName
  [[sym symbol?]])

(defexpr BlockName CodeName
  [[num nat-int?]])

(define-language Insn)

(defexpr Copy Insn
  [[into ::LocalVariable]
   [from ::Value]])

(defexpr Ret Insn
  [[value ::Value]])

(defexpr Call Insn
  [[into (spec/nilable ::LocalVariable)]
   [operator (spec/or ::Value ::CodeName)]
   [operands (spec/nilable (spec/coll-of ::Value))]])

(defexpr TailCall Insn
  [[operator ::Value]
   [operands (spec/nilable (spec/coll-of ::Value))]])

(defexpr CallIf Insn
  [[into (spec/nilable ::LocalVariable)]
   [cond ::Value]
   [then ::Value]
   [else ::Value]])

(defexpr TailCallIf Insn
  [[cond ::Value]
   [then ::Value]
   [else ::Value]])

(defexpr MakeClosure Insn
  [[into ::LocalVariable]
   [code ::FunctionName]
   [closure-env (spec/map-of ::ClosureVariable ::Value)]])

(defexpr SetClosure Insn
  [[closure ::LocalVariable]
   [var-to-set ::ClosureVariable]
   [new-value ::Value]])

(define-language Function')

(defexpr Function Function'
  [[name ::FunctionName]
   [arglist (spec/nilable (spec/coll-of ::LocalVariable))]
   [closure-env (spec/coll-of ::ClosureVariable)]
   [insns (spec/and vector? (spec/coll-of ::Insn))]
   [child-functions (spec/map-of ::FunctionName ::Function)]])

(defn- gen-local
  ([name]
   {:post [(spec/assert ::LocalVariable %)]}
   (make-localvariable (gensym (str name))))
  ([]
   (gen-local "temp-")))

(defn- gen-function-name []
  {:post [(spec/assert ::FunctionName %)]}
  (make-functionname (gensym)))

(defn- extend-function [fun & new-insns]
  {:pre [(spec/assert ::Function fun)
         (spec/assert (spec/coll-of ::Insn) new-insns)]
   :post [(spec/assert ::Function %)]}
  (update-insns fun
                #(apply vector (concat % new-insns))))

(defn- add-local-functions [parent & children]
  {:pre [(spec/assert ::Function parent)
         (spec/assert (spec/coll-of ::Function) children)]
   :post [(spec/assert ::Function %)]}
  (update-child-functions parent
                          #(reduce (fn [partial-children next-child]
                                     (assoc partial-children (name next-child) next-child))
                                   %
                                   children)))

;;; multimethods for mutually recursive transformation

(def ^:private transforms-like (-> (make-hierarchy)
                                   (derive ::ltr/Lit ::SimpleValue)
                                   (derive ::ltr/LocalName ::SimpleValue)
                                   (derive ::ltr/ClosureName ::SimpleValue)
                                   (derive ::ltr/Uninit ::SimpleValue)
                                   (derive ::ltr/Fn ::GoesInVariable)
                                   (derive ::ltr/Let ::TransformsRecursively)
                                   (derive ::ltr/Begin ::TransformsRecursively)))

(defmulti ^:private transform-to-value' (fn [expr] (:variant expr))
  :hierarchy #'transforms-like)

(defn- transform-to-value [expr]
  {:pre [(spec/assert ::ltr/Expr expr)]
   :post [(spec/assert ::Value %)]}
  (transform-to-value' expr))

(defmulti ^:private transform-to-var' (fn [expr _ _] (:variant expr))
  :hierarchy #'transforms-like)

(defn- transform-to-var [expr into fun]
  {:pre [(spec/assert ::ltr/Expr expr)
         (spec/assert ::LocalVariable into)
         (spec/assert ::Function fun)]
   :post [(spec/assert ::Function %)]}
  (transform-to-var' expr into fun))

(defmulti ^:private transform-to-return' (fn [expr _] (:variant expr))
  :hierarchy #'transforms-like)

(defn- transform-to-return [expr fun]
  {:pre [(spec/assert ::ltr/Expr expr)
         (spec/assert ::Function fun)]
   :post [(spec/assert ::Function %)]}
  (transform-to-return' expr fun))

(defmulti ^:private transform-to-discard' (fn [expr _] (:variant expr))
  :hierarchy #'transforms-like)

(defn- transform-to-discard [expr fun]
  {:pre [(spec/assert ::ltr/Expr expr)
         (spec/assert ::Function fun)]
   :post [(spec/assert ::Function %)]}
  (transform-to-discard' expr fun))

(defmulti ^:private transform-recursively' (fn [expr _ _] (:variant expr))
  :hierarchy #'transforms-like)

(defn- transform-recursively [expr transform-child-fn fun]
  {:pre [(spec/assert ::ltr/Expr expr)
         (spec/assert fn? transform-child-fn)
         (spec/assert ::Function fun)]
   :post [(spec/assert ::Function %)]}
  (transform-recursively' expr transform-child-fn fun))

;;; transforming functions

(defn- transform-function [fun]
  {:pre [(spec/assert ::ltr/Fn fun)]
   :post [(spec/assert ::Function %)]}
  (let [arglist (map #(make-localvariable (ltr/sym %)) (ltr/arglist fun))
        closure-env (clojure.core/into [] (map (fn [[inner-name _]]
                                                 (transform-to-value inner-name))
                                               (ltr/closure-env fun)))
        empty-function (make-function (if-let [name (ltr/name fun)]
                                        (make-functionname (ltr/sym name))
                                        (gen-function-name))
                                      arglist
                                      closure-env
                                      []
                                      {})]
    (transform-to-return (ltr/body-expr fun) empty-function)))

;;; transforming SimpleValues, i.e. ltr/Lit and ltr/Name

(defmethod transform-to-value' ::ltr/Lit [lit]
  (make-lit (ltr/value lit)))

(defmethod transform-to-value' ::ltr/LocalName [name]
  (make-localvariable (ltr/sym name)))

(defmethod transform-to-value' ::ltr/ClosureName [name]
  (make-closurevariable (ltr/sym name)))

(defmethod transform-to-value' ::ltr/Uninit [_]
  uninit)

(defmethod transform-to-var' ::SimpleValue [val into fun]
  (extend-function fun (make-copy into (transform-to-value val))))

(defmethod transform-to-return' ::SimpleValue [val fun]
  (extend-function fun (make-ret (transform-to-value val))))

(defmethod transform-to-discard' ::SimpleValue [_ fun]
  fun)

;; transforming things that go in variables, i.e. ltr/Fn

(defn- bind-params [fun arg-exprs arg-temps]
  {:pre [(spec/assert ::Function fun)
         (spec/assert (spec/coll-of ::ltr/Expr) arg-exprs)
         (spec/assert (spec/coll-of ::LocalVariable) arg-temps)
         (= (count arg-exprs) (count arg-temps))]
   :post [(spec/assert ::Function fun)]}
  (reduce (fn [partial-fun [next-arg-expr next-arg-temp]]
            (transform-to-var next-arg-expr next-arg-temp partial-fun))
          fun
          (map vector arg-exprs arg-temps)))

(defmethod transform-to-var' ::ltr/Fn [closure into fun]
  (let [child (transform-function closure)
        closure-env (clojure.core/into {} (map (fn [[inner-name outer-name]]
                                                 [(transform-to-value inner-name)
                                                  (transform-to-value outer-name)])
                                               (ltr/closure-env closure)))]
    (add-local-functions (extend-function fun
                                          (make-makeclosure into
                                                            (name child)
                                                            closure-env))
                         child)))

(defmethod transform-to-return' ::GoesInVariable [varable fun]
  (let [temp (gen-local)
        with-in-var (transform-to-var varable temp fun)]
    (extend-function with-in-var (make-ret temp))))

(defmethod transform-to-discard' ::GoesInVariable [_ fun]
  fun)

;; transforming calls

(defn- prep-call-then [call fun then]
  (let [fn-temp (gen-local "fn-")
        arg-temps (map (fn [_] (gen-local "arg-")) (ltr/operands call))
        with-args-bound (bind-params fun
                                     (cons (ltr/operator call) (ltr/operands call))
                                     (cons fn-temp arg-temps))]
    (then with-args-bound fn-temp arg-temps)))

(defmethod transform-to-return' ::ltr/Call [call fun]
  (prep-call-then call fun
                  (fn [fun fn-temp arg-temps]
                    (extend-function fun (make-tailcall fn-temp arg-temps)))))

(defmethod transform-to-var' ::ltr/Call [call into fun]
  (prep-call-then call fun
                  (fn [fun fn-temp arg-temps]
                    (extend-function fun (make-call into fn-temp arg-temps)))))

(defmethod transform-to-discard' ::ltr/Call [call fun]
  (prep-call-then call fun
                  (fn [fun fn-temp arg-temps]
                    (extend-function fun (make-call nil fn-temp arg-temps)))))

;; transforming conditional calls

(defmethod transform-to-return' ::ltr/CallIf [callif fun]
  (extend-function fun (make-tailcallif (transform-to-value (ltr/condition callif))
                                        (transform-to-value (ltr/then callif))
                                        (transform-to-value (ltr/else callif)))))

(defmethod transform-to-var' ::ltr/CallIf [callif into fun]
  (extend-function fun (make-callif into
                                    (transform-to-value (ltr/condition callif))
                                    (transform-to-value (ltr/then callif))
                                    (transform-to-value (ltr/else callif)))))

(defmethod transform-to-discard' ::ltr/CallIf [callif fun]
  (extend-function fun (make-callif nil
                                    (transform-to-value (ltr/condition callif))
                                    (transform-to-value (ltr/then callif))
                                    (transform-to-value (ltr/else callif)))))

;; transforming assignments generated by letrec

(defmethod transform-to-discard' ::ltr/SetClosure [setclosure fun]
  (extend-function fun (make-setclosure (transform-to-value (ltr/target setclosure))
                                        (transform-to-value (ltr/name setclosure))
                                        (transform-to-value (ltr/new-value setclosure)))))

;; transforming scoped constructs recursively, i.e. Begin, Let, If

(defmethod transform-recursively' ::ltr/Let [lt transform-body fun]
  (let [var (transform-to-value (ltr/name lt))
        with-binding (transform-to-var (ltr/initform lt) var fun)]
    (transform-body (ltr/body-expr lt) with-binding)))

(defmethod transform-recursively' ::ltr/Begin [begin transform-body fun]
  (->> fun
       (transform-to-discard (ltr/first begin))
       (transform-body (ltr/second begin))))

(defmethod transform-to-discard' ::TransformsRecursively [expr fun]
  (transform-recursively expr transform-to-discard fun))

(defmethod transform-to-var' ::TransformsRecursively [expr into fun]
  (transform-recursively expr
                         (fn [body fun]
                           (transform-to-var body into fun))
                         fun))

(defmethod transform-to-return' ::TransformsRecursively [expr fun]
  (transform-recursively expr transform-to-return fun))

;; entry point

(defn transform-program [prog]
  {:pre [(spec/assert ::ltr/Expr prog)]
   :post [(spec/assert ::Function %)]}
  (transform-function (ltr/make-fn (ltr/make-localname 'main)
                                   []
                                   {}
                                   prog)))
