(ns toylang.threeval
  (:gen-class)
  (:refer-clojure :exclude [eval])
  (:require [toylang.threeaddr :as ir]
            [toylang.defexpr :refer [defexpr define-language spec-assert-then-true]]
            [clojure.pprint :as pretty]
            [clojure.spec.alpha :as spec])
  (:use [slingshot.slingshot :only [throw+ try+]]))

(def ^:private uninit :uninit)

(defn- uninit? [thing]
  (= thing uninit))

(spec/def ::ClosureEnv (spec/map-of ::ir/ClosureVariable #(instance? clojure.lang.IAtom %)))

(spec/def ::Value (spec/or ::Bool boolean?
                           ::Int #(instance? java.lang.Long %)
                           ::String string?
                           ::Uninit uninit?
                           ::Closure (spec/tuple ::ir/Function ::ClosureEnv)))

(spec/def ::LocalEnv (spec/map-of ::ir/LocalVariable ::Value))

(spec/def ::InstructionPointer nat-int?)

(define-language StackFrame')

(defexpr StackFrame StackFrame'
  [[ip ::InstructionPointer]
   [function ::ir/Function]
   [closure-env ::ClosureEnv]
   [local-env ::LocalEnv]
   [return-into (spec/nilable ::ir/LocalVariable)]])

(spec/def ::Stack (spec/and list? (spec/coll-of ::StackFrame)))

(defn- get-next-insn [stack-frame]
  {:pre [(spec/assert ::StackFrame stack-frame)]
   :post [(spec/assert ::ir/Insn %)]}
  (nth (ir/insns (function stack-frame)) (ip stack-frame)))

(defn- increment-ip [stack-frame]
  {:pre [(spec/assert ::StackFrame stack-frame)]
   :post [(spec/assert ::StackFrame %)]}
  (update-ip stack-frame #(+ % 1)))

(defn- set-local [stack-frame local-var value]
  {:pre [(spec/assert ::StackFrame stack-frame)
         (spec/assert ::ir/LocalVariable local-var)
         (spec-assert-then-true ::Value value)]
   :post [(spec/assert ::StackFrame %)]}
  (update-local-env stack-frame
                    (fn [local-env]
                      (assoc local-env
                             local-var
                             value))))

(defn- make-fresh-stack-frame [function & {:keys [local-env closure-env]
                                           :or {local-env {}
                                                closure-env {}}}]
  {:pre [(spec/assert ::ir/Function function)
         (spec/assert ::LocalEnv local-env)
         (spec/assert ::ClosureEnv closure-env)]
   :post [(spec/assert ::StackFrame %)]}
  (make-stackframe 0
                   function
                   closure-env
                   local-env
                   nil))

(defmulti ^:private eval-insn (fn [insn _] (:variant insn)))

(defn eval [program]
  {:pre [(spec/assert ::ir/Function program)]
   :post [(spec-assert-then-true ::Value %)]}
  (letfn [(eval-loop [[current-frame & stack]]
            (let [next-insn (get-next-insn current-frame)
                  frame-post-ip-inc (increment-ip current-frame)
                  new-stack (eval-insn next-insn (cons frame-post-ip-inc stack))]
              (recur new-stack)))]
    (let [stack [(make-fresh-stack-frame program)]]
      (try+ (eval-loop stack)
            (catch [:type ::ReturnExit] {:keys [value]}
              value)))))

(defmulti ^:private resolve-value' (fn [val _] (:variant val)))

(defn- resolve-value [val stack-frame]
  {:pre [(spec/assert ::ir/Value val)
         (spec/assert ::StackFrame stack-frame)]
   :post [(spec-assert-then-true ::Value %)]}
  (resolve-value' val stack-frame))

(defmethod resolve-value' ::ir/Lit [lit _]
  (ir/value lit))

(defmethod resolve-value' ::ir/LocalVariable [local frame]
  (let [val (get (local-env frame) local uninit)]
    (if (uninit? val)
      (throw+ {:type ::UninitializedLocalVariable
               :var local
               :frame frame})
      val)))

(defmethod resolve-value' ::ir/ClosureVariable [closure frame]
  (let [val (get (closure-env frame) closure uninit)]
    (cond (uninit? val) (throw+ {:type ::UnknownClosureVariable
                                 :var closure
                                 :frame frame})
          (uninit? (deref val)) (throw+ {:type ::UninitializedClosureVariable
                                         :var closure
                                         :frame frame})
          :else (deref val))))

;; initializing closure envs may reference uninit, but nowhere else; a variable may never be bound to uninit
(defmethod resolve-value' ::ir/Uninit [_ _]
  uninit)

(defmethod eval-insn ::ir/Copy [copy [current-frame & stack]]
  (cons (set-local current-frame (ir/into copy) (resolve-value (ir/from copy) current-frame))
          stack))

(defn- return-to-parent-frame [value current-frame parent-frame & stack]
  {:pre [(spec-assert-then-true ::Value value)
         (spec/assert ::StackFrame current-frame)
         (spec/assert ::StackFrame parent-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (cons (if-let [into-local (return-into parent-frame)]
          (set-local parent-frame into-local value)
          parent-frame)))

(defn- return-exit-program [val]
  {:pre [(spec-assert-then-true ::Value val)]
   :post [false]}
  (throw+ {:type ::ReturnExit :value val}))

(defmethod eval-insn ::ir/Ret [ret [current-frame & stack]]
  {:pre [(spec/assert ::ir/Ret ret)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [val (resolve-value (ir/value ret) current-frame)]
    (if (empty? stack)
      (return-exit-program val)
      (apply return-to-parent-frame val current-frame stack))))

(defn- make-child-local-env [fun call-like parent-frame]
  (into {} (map (fn [arg-name unresolved-operand]
                  [arg-name (resolve-value unresolved-operand parent-frame)])
                (ir/arglist fun)
                (ir/operands call-like))))

(defn- resolve-to-closure [val stack-frame]
  {:pre [(spec/assert ::ir/Value val)
         (spec/assert ::StackFrame stack-frame)]
   :post [(spec/assert ::Closure %)]}
  (resolve-value val stack-frame))

(defmethod eval-insn ::ir/Call [call [current-frame & stack]]
  {:pre [(spec/assert ::ir/Call call)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [[fun closure-env] (resolve-to-closure (ir/operator call) current-frame)
        arg-locals (make-child-local-env fun call current-frame)
        parent-frame (replace-return-into current-frame
                                          (ir/into call))
        child-frame (make-fresh-stack-frame fun
                                            :closure-env closure-env
                                            :local-env arg-locals)]
    (list* child-frame parent-frame stack)))

(defmethod eval-insn ::ir/TailCall [tailcall [current-frame & stack]]
  {:pre [(spec/assert ::ir/TailCall tailcall)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [[fun closure-env] (resolve-to-closure (ir/operator tailcall) current-frame)
        arg-locals (make-child-local-env fun tailcall current-frame)
        new-frame (make-fresh-stack-frame fun
                                          :closure-env closure-env
                                          :local-env arg-locals)]
    (list* new-frame stack)))

(defmethod eval-insn ::ir/CallIf [callif [current-frame & stack]]
  {:pre [(spec/assert ::ir/CallIf callif)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [cond (resolve-value (ir/cond callif))
        call-clause (fn [clause]
                      (let [[fun closure-env] (resolve-to-closure clause)
                            child-frame (make-fresh-stack-frame fun
                                                                :closure-env closure-env)
                            parent-frame (replace-return-into current-frame
                                                              (ir/into callif))]
                        (list* child-frame parent-frame stack)))]
    (if cond
      (call-clause (ir/then callif))
      (call-clause (ir/else callif)))))

(defmethod eval-insn ::ir/TailCallIf [tailcallif [current-frame & stack]]
  {:pre [(spec/assert ::ir/TailCallIf tailcallif)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [cond (resolve-value (ir/cond tailcallif) current-frame)
        call-clause (fn [clause]
                      (let [[fun closure-env] (resolve-to-closure clause current-frame)
                            new-frame (make-fresh-stack-frame fun
                                                              :closure-env closure-env)]
                        (list* new-frame stack)))]
    (if cond
      (call-clause (ir/then tailcallif))
      (call-clause (ir/else tailcallif)))))

(defn- find-child-function [fname parent]
  {:pre [(spec/assert ::ir/FunctionName fname)
         (spec/assert ::ir/Function parent)]
   :post [(spec/assert ::ir/Function %)]}
  (get (ir/child-functions parent) fname))

(defmethod eval-insn ::ir/MakeClosure [makeclosure [current-frame & stack]]
  {:pre [(spec/assert ::ir/MakeClosure makeclosure)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [fun (find-child-function (ir/code makeclosure) (function current-frame))
        closure-env (into {} (map (fn [[closure-var val]]
                                    [closure-var (atom (resolve-value val current-frame))])
                                  (ir/closure-env makeclosure)))]
    (cons (set-local current-frame
                     (ir/into makeclosure)
                     (spec/assert ::Closure [fun closure-env]))
          stack)))

(defmethod eval-insn ::ir/SetClosure [setclosure [current-frame & stack]]
  {:pre [(spec/assert ::ir/SetClosure setclosure)
         (spec/assert ::StackFrame current-frame)
         (spec-assert-then-true (spec/nilable (spec/coll-of ::StackFrame)) stack)]
   :post [(spec/assert (spec/coll-of ::StackFrame) %)]}
  (let [[fun closure-env] (resolve-to-closure (ir/closure setclosure) current-frame)
        new-value (resolve-value (ir/new-value setclosure) current-frame)]
    (if-let [place (get closure-env (ir/var-to-set setclosure))]
      (reset! place new-value)
      (throw+ {:type ::UnknownClosureVar :var (ir/var-to-set setclosure) :env closure-env :in (ir/closure setclosure)}))
    (cons current-frame stack)))
