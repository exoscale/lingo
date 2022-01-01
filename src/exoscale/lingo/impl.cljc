(ns exoscale.lingo.impl
  (:refer-clojure :exclude [format])
  (:require [clojure.walk :as walk]
            [clojure.spec.alpha :as s]
            #?@(:cljs [[goog.string]
                       [goog.string.format]])))

(def format
  #?(:clj clojure.core/format
     :cljs goog.string/format))

(defn spec-error-message
  [spec registry-val]
  (get-in registry-val [:exoscale.lingo/spec-msg spec]))

(defn strip-core
  [sym]
  (cond-> sym
    (= (namespace sym) "clojure.core")
    (-> name symbol)))

(defn abbrev [form]
  (cond->> form
    (seq? form)
    (walk/postwalk (fn [form]
                     (let [qs? (qualified-symbol? form)]
                       (cond
                         ;; just treat */% as %
                         (and qs? (= "%" (name form)))
                         (symbol "%")

                         ;; it's could be a core symbol, in that case remove ns
                         qs?
                         (strip-core form)

                         (and (seq? form)
                              (contains? #{'fn 'fn*} (first form)))
                         (last form)
                         :else form))))))

(defn parent-spec
  "Look up for the parent spec using the spec hierarchy."
  [k]
  (when-let [p (some-> k s/get-spec)]
    (or (when (qualified-ident? p) p)
        (s/form p))))

(defn spec-vals
  "Returns all spec keys or pred "
  ([spec-ident]
   (->> spec-ident
        (iterate parent-spec)
        (take-while some?))))

(defn find-pred-data
  [pred {:exoscale.lingo/keys [registry conform] :as _opts}]
  (reduce (fn [_ k]
            (when-let [match (conform k (abbrev pred))]
              (when (not= match :clojure.spec.alpha/invalid)
                (reduced #:exoscale.lingo.pred{:spec k
                                               :vals match}))))
          nil
          (get @registry :exoscale.lingo/pred-conformer)))

(defn find-ident-data
  [spec {:as _opts :exoscale.lingo/keys [registry]}]
  (let [registry-val @registry
        spec-path (spec-vals spec)]
    (reduce (fn [_ spec]
              (let [spec' (abbrev spec)]
                (when-let [msg (spec-error-message spec' registry-val)]
                  (reduced #:exoscale.lingo.ident{:msg msg
                                                  :spec spec'
                                                  :path spec-path}))))
            nil
            spec-path)))

(defn path-str
  [in]
  (when (seq in)
    (letfn [(mdot [s] (when (seq s) (str ".")))]
      (reduce (fn [s segment]
                (str s
                     (cond
                       (nat-int? segment)
                       (format "[%d]" segment)

                       (keyword? segment)
                       (str (mdot s) (name segment))

                       :else
                       (str (mdot s) (str segment)))))
              nil
              in))))
