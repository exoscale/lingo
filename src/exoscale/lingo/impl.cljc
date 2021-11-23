(ns exoscale.lingo.impl
  (:require [clojure.walk :as walk]
            [clojure.spec.alpha :as s]))

(defn spec-error-message
  [spec registry-val]
  (get-in registry-val [:exoscale.lingo/specs spec]))

(defn strip-core
  [sym]
  (cond-> sym
    (= (namespace sym) "clojure.core")
    (-> name symbol)))

(defn find-registry-pred-message
  [x {:as _opts :exoscale.lingo/keys [registry conform]}]
  (reduce (fn [_ [k formater]]
            (when-let [match (conform k x)]
              (when (not= match :clojure.spec.alpha/invalid)
                (reduced (formater match)))))
          nil
          (get @registry :exoscale.lingo/preds)))

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
                              (= 'fn (first form)))
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

(defn find-pred-error-message
  [pred {:exoscale.lingo/keys [registry] :as opts}]
  (if (qualified-ident? pred)
    (spec-error-message (abbrev pred) @registry)
    (find-registry-pred-message (abbrev pred)
                                opts)))

(defn find-ident-error-message
  [spec {:as _opts :exoscale.lingo/keys [registry]}]
  (let [registry-val @registry]
    (reduce (fn [_ spec]
              (when-let [msg (spec-error-message (abbrev spec) registry-val)]
                (reduced msg)))
            nil
            (spec-vals spec))))

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
