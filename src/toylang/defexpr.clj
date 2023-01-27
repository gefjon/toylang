(ns toylang.defexpr
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as spec]))

(defn make-map [& key-value-pairs]
  (let [add-one (fn [partial-map [key value]]
                  (assoc! partial-map key value))]
    (persistent! (reduce add-one (transient {}) key-value-pairs))))

(defmacro defexpr [name [& fields]]
  (let [keywordify (fn [name]
                     (keyword (str (ns-name *ns*)) (str name)))
        variant-name (keywordify name)
        field-name (fn [[field-name _ & _]] field-name)
        field-keyword (fn [field]
                        (keywordify (str name "/" (field-name field))))
        field-spec (fn [[_ field-spec & _]] field-spec)
        field-optional? (fn [field]
                          (= :optional (nth field 3 nil)))
        optional-field-keys (map field-keyword (filter field-optional? fields))
        required-field-keys (map field-keyword (filter (complement field-optional?) fields))
        ctor-name (symbol (str "make-" (str/lower-case name)))
        pred-name (symbol (str name "?"))]
    `(do ~@(map (fn [field] `(spec/def ~(field-keyword field)
                               ~(field-spec field)))
                fields)
         (spec/def ~variant-name (spec/and #(= (:variant %) ~variant-name)
                                           (spec/keys :req ~required-field-keys
                                                      :opt ~optional-field-keys)))
         (defn ~pred-name [thing#]
           (spec/valid? ~variant-name thing#))
         (defn ~ctor-name [~@(map field-name fields)]
           {:pre ~(into [] (map (fn [field]
                                  `(spec/assert ~(field-keyword field) ~(field-name field)))
                                fields))
            :post [(spec/assert ~variant-name ~'%)]}
           (make-map [:variant ~variant-name]
                     ~@(map (fn [field]
                              `[~(field-keyword field) ~(field-name field)])
                            fields))))))

(defn variant [thing]
  (:variant thing))
