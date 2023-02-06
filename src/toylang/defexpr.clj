(ns toylang.defexpr
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as spec]))

(defn make-map [& key-value-pairs]
  (let [add-one (fn [partial-map [key value]]
                  (assoc! partial-map key value))]
    (persistent! (reduce add-one (transient {}) key-value-pairs))))

(defn keywordify [& name]
  (keyword (str (ns-name *ns*)) (apply str name)))

(spec/def ::Field (spec/tuple symbol? (constantly true)))

(defn field-name [field]
  {:pre [(spec/assert ::Field field)]
   :post [(spec/assert symbol? %)]}
  (first field))

(defn field-spec [field]
  {:pre [(spec/assert ::Field field)]}
  (second field))

(defn field-keyword [language variant field]
  {:pre [(spec/assert ::Field field)]
   :post [(spec/assert keyword? %)]}
  (keywordify language "/" variant "/" (field-name field)))

(defn variant-keyword [variant]
  {:post [(spec/assert keyword? %)]}
  (keywordify variant))

(defn language-spec-symbol [language]
  (symbol (apply str language "-spec")))

(defn language-spec-keyword [language]
  (keywordify language))

(defn accessor-mm-name [field]
  (field-name field))

(defn replacer-mm-name [field]
  (symbol (str "replace-" (field-name field))))

(defn updater-mm-name [field]
  (symbol (str "update-" (field-name field))))

(defn spec-assert-then-true [spec value]
  "Like `spec/assert`, but always returns `true` if the assertion succeeds.

For use in `:pre` and `:post` conditions, because e.g. `(spec/assert boolean? false)` will
pass the assertion, return false, and then fail the pre/post-condition."
  (spec/assert spec value)
  true)

(defmacro define-accessor [language variant field]
  `(do (defmulti ~(accessor-mm-name field) :variant)
       (defmethod ~(accessor-mm-name field) ~(variant-keyword variant) [instance#]
         {:pre [(spec-assert-then-true ~(variant-keyword variant) instance#)]
          :post [(spec-assert-then-true ~(field-keyword language variant field) ~'%)]}
         (~(field-keyword language variant field) instance#))))

(defmacro define-replacer [language variant field]
  (let [mm-name (replacer-mm-name field)]
    `(do (defmulti ~mm-name (fn [instance# _#] (:variant instance#)))
         (defmethod ~mm-name ~(variant-keyword variant) [instance# new-value#]
           {:pre [(spec-assert-then-true ~(variant-keyword variant) instance#)
                  (spec-assert-then-true ~(field-keyword language variant field) new-value#)]
            :post [(spec-assert-then-true ~(variant-keyword variant) ~'%)]}
           (assoc instance# ~(field-keyword language variant field) new-value#)))))

(defmacro define-updater [language variant field]
  (let [mm-name (updater-mm-name field)]
    `(do (defmulti ~mm-name (fn [instance# _#] (:variant instance#)))
         (defmethod ~mm-name ~(variant-keyword variant) [instance# fun#]
           {:pre [(spec-assert-then-true ~(variant-keyword variant) instance#)]
            :post [(spec-assert-then-true ~(variant-keyword variant) ~'%)]}
           (update instance# ~(field-keyword language variant field) fun#)))))

(defmacro define-field-spec [language variant field]
  `(spec/def ~(field-keyword language variant field)
     ~(field-spec field)))

(defmacro define-variant-spec-method [language variant]
  `(defmethod ~(language-spec-symbol language) ~(variant-keyword variant) [~'_]
     ~(variant-keyword variant)))

(defmacro defexpr [name language [& fields]]
  (let [required-field-specs (map #(field-keyword language name %)
                                  fields)
        ctor-name (symbol (str "make-" (str/lower-case name)))]
    `(do ~@(map (fn [field] `(do (define-field-spec ~language ~name ~field)
                                 (define-accessor ~language ~name ~field)
                                 (define-replacer ~language ~name ~field)
                                 (define-updater ~language ~name ~field)))
                fields)
         (spec/def ~(variant-keyword name) (spec/and #(= (:variant %) ~(variant-keyword  name))
                                                     (spec/keys :req ~required-field-specs)))
         (define-variant-spec-method ~language ~name)
         (defn ~ctor-name [~@(map field-name fields)]
           {:pre ~(into [] (map (fn [field]
                                  `(spec-assert-then-true ~(field-keyword language name field) ~(field-name field)))
                                fields))
            :post [(spec-assert-then-true ~(variant-keyword name) ~'%)]}
           (make-map [:variant ~(variant-keyword name)]
                     ~@(map (fn [field]
                              `[~(field-keyword language name field) ~(field-name field)])
                            fields))))))

(defmacro define-language [name]
  `(do (defmulti ~(language-spec-symbol name) :variant)
       (defmethod ~(language-spec-symbol name) nil [~'_] (constantly false))
       (spec/def ~(language-spec-keyword name)
         (spec/multi-spec ~(language-spec-symbol name) :variant))))

(defn variant [thing]
  (:variant thing))
