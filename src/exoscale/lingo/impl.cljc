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
  (get-in registry-val [:exoscale.lingo.registry.spec/message spec]))

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
  [pred {:keys [registry conform] :as _opts}]
  (reduce (fn [_ k]
            (when-let [match (conform k (abbrev pred))]
              (when (not= match :clojure.spec.alpha/invalid)
                (reduced #:exoscale.lingo.explain.pred{:spec k
                                                       :vals match}))))
          nil
          (:exoscale.lingo.registry.pred/conformers @registry)))

(defn find-spec-data
  [spec {:as _opts :keys [registry]}]
  (let [registry-val @registry]
    (reduce (fn [_ spec]
              (let [spec' (abbrev spec)]
                (when-let [message (spec-error-message spec' registry-val)]
                  (reduced #:exoscale.lingo.explain.spec{:message message
                                                         :spec spec'}))))
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

;;; grouping

(defn- missing-keys-pbs-by-path [pbs]
  (not-empty
   (group-by (fn [{:as _pb :keys [path]}] path)
             (filter #(= (:exoscale.lingo.explain.pred/spec %)
                         :exoscale.lingo.pred/contains-key)
                     pbs))))

(defn group-missing-keys
  [pbs]
  (when-let [mk-by-path (missing-keys-pbs-by-path pbs)]
    (let [missing-keys-pbs (into #{}
                                 (comp (map val) cat)
                                 mk-by-path)]
      (concat (remove (fn [pb] (contains? missing-keys-pbs pb)) pbs)
              (map (fn [pbs]
                     (let [missing-keys (into #{}
                                              (map #(-> %
                                                        :exoscale.lingo.explain.pred/vals
                                                        :key))
                                              pbs)]
                       (-> (first pbs)
                           (select-keys [:path :via :val :in])
                           (assoc :pred (list  'contains-keys? '% missing-keys)
                                  :exoscale.lingo.explain.pred/spec :exoscale.lingo.pred/contains-keys
                                  :exoscale.lingo.explain.pred/vals {:keys missing-keys}))))
                   (vals mk-by-path))))))
