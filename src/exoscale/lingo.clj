(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            exoscale.specs.string
            exoscale.specs.net
            [exoscale.specs :as xs]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [meander.epsilon :as m]))

(defn with-name!
  "Adds custom name to a spec"
  [spec name]
  (xs/vary-meta! spec
                 assoc :exoscale.lingo/name name))

(defn with-error!
  "Adds custom error message for a spec"
  [spec error-msg]
  (xs/vary-meta! spec
                 assoc :exoscale.lingo/error error-msg))

;; set defaults for common idents

(with-name! `string? "String")
(with-name! `char? "Character")
(with-name! `map? "Map")
(with-name! `coll? "Collection")
(with-name! `set? "Set")
(with-name! `vector? "Vector")
(with-name! `associative? "Associative (map, vector)")
(with-name! `sequential?  "Sequential")
(with-name! `number? "Number")
(with-name! `bytes? "Bytes")
(with-name! `float? "Float")
(with-name! `double? "Double")
(with-name! `boolean? "Boolean")
(with-name! `true? "true")
(with-name! `false? "false")
(with-name! `zero? "Zero")
(with-name! `empty? "Empty")
(with-name! `ident? "Identifier (keyword or symbol)")
(with-name! `qualified-ident? "Qualified Identifier (keyword or symbol)")
(with-name! `symbol? "Symbol")
(with-name! `uuid? "UUID")
(with-name! `uri? "URI")
(with-name! `int? "Integer")
(with-name! `nat-int? "Integer")
(with-name! `pos-int? "Positive Integer")
(with-name! `neg-int? "Negative Integer")
(with-name! `pos? "Positive number")
(with-name! `neg? "Negative number")
(with-name! `inst? "Instant")
(with-name! `some? "Non-nil")
(with-name! `nil? "nil")

(defn spec-name
  [spec]
  (get (xs/meta spec) :exoscale.lingo/name))

(defn spec-error-message
  [spec]
  (get (xs/meta spec) :exoscale.lingo/error))

(defn spec-str
  [spec]
  (or (spec-name spec)
      (pr-str spec)))

(defn strip-core
  [sym]
  (cond-> sym
    (= (namespace sym) "clojure.core")
    (-> name symbol)))

(s/def ::contains
  (s/cat :pred #{'contains?}
         :arg any?
         :key any?))

(s/def ::set set?)
(s/def ::ident ident?)

(s/def ::min-count
  (s/cat :_op #{'<=}
         :min number?
         :_cnt #{'(count %)}
         :_max #{'Integer/MAX_VALUE}))

(s/def ::max-count
  (s/cat :_op #{'>=}
         :_zero zero?
         :_cnt #{'(count %)}
         :max number?))

(s/def ::between-count
  (s/cat :_op #{<=}
         :min number?
         :_cnt #{'(count %)}
         :max number?))

(s/def ::compare-op
  (s/or :count-1
        (s/cat :op #{'= '< '> '<= '>= 'not=}
               :_ #{'(count %)}
               :x any?)
        :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                        :count number?
                        :_ #{'(count %)})))

(s/def ::int-in
  (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
         :min number?
         :max number?
         :_ #{'%}))

;; (s/conform ::compare-op '(>= 1 (count %)))

(def matchers-registry (atom {}))

(defn register-matcher!
  ([spec-key f]
   (register-matcher! matchers-registry spec-key f))
  ([registry spec-key f]
   (swap! registry assoc spec-key f)))

(defn find-matcher-message
  [registry x]
  (reduce (fn [_ [k formater]]
            (prn k)
            (let [match (and (s/get-spec k) (s/conform k x))]
              (when (not= match :clojure.spec.alpha/invalid)
                (reduced (formater match)))))
          nil
          @registry))

(register-matcher! matchers-registry ::contains #(format "missing key %s" (:key %)))
(register-matcher! matchers-registry ::set #(format "should be one of %s" (clojure.string/join "," %)))
(register-matcher! matchers-registry ::min-count #(format "should contain at least %s elements"
                                                          (:min %)))
(register-matcher! matchers-registry ::max-count #(format "should contain at most %s elements"
                                                          (:min %)))

(register-matcher! matchers-registry ::between-count #(format "should contain between %s %s elements"
                                                              (:min %)
                                                              (:max %)))

(register-matcher! matchers-registry ::compare-op
                   (fn [[_ {:keys [op x]}]]
                     (format "should contain %s %s element(s)"
                             (case op
                               not= "not="
                               = "="
                               > ">"
                               < "<"
                               >= ">="
                               <= "<=")
                             x)))

(defn- abbrev [form]
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

(defn- parent-spec
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

(defn pred-str
  [pred registry]
  (find-matcher-message registry
                        (abbrev pred)))

(xs/meta :exoscale.lingo.test.core-test/things)
(defn find-error-message
  "Given a spec named `k`, return its human-readable error message."
  [problem k pred-matcher]
  (prn :sv (spec-vals k))
  (reduce (fn [_ k]
            (prn :k k)
            (if (qualified-keyword? k)
              (when-let [msg (spec-error-message k)]
                (reduced msg))
              (when-let [msg (-> problem :pred (pred-str pred-matcher))]
                (reduced msg))))
          nil
          (spec-vals k)))

(defn explain-data
  ([spec value]
   (explain-data spec value nil))
  ([spec value {:as _opts
                :exoscale.lingo/keys [registry]
                :or {registry matchers-registry}}]
   (some-> (s/explain-data spec value)
           (update :clojure.spec.alpha/problems
                   (fn [pbs]
                     (map (fn [{:keys [pred _val _reason via _in _spec _path] :as pb}]
                            (let [spec (or (last via) pred)]
                              (assoc pb
                                     :exoscale.lingo/message (find-error-message pb
                                                                                 spec
                                                                                 registry))))
                          (->> pbs
                               (sort-by #(- (count (:path %)))))))))))

(defn explain*
  [spec value opts]
  (if-let [{:as _ed :clojure.spec.alpha/keys [problems]} (explain-data spec value opts)]
    (doseq [{:as _problem
             :exoscale.lingo/keys [message]
             :keys [via in val]} problems
            :let [spec (last via)]]
      (print (pr-str val))

      (when-not (empty? in)
        (print (format " in `%s`" (pr-str in))))

      (if spec
        (print (format " is an invalid %s" (spec-str spec)))
        (print " is invalid"))

      (print " - ")
      (print message)
      (newline))

    (println "Success!")))

(defn explain-str
  "Like spec explain-str, but uses lingo printer"
  ([spec x] (explain-str spec x nil))
  ([spec x opts]
   (with-out-str
     (explain* spec x opts))))

(defn explain
  "Like spec explain, but uses lingo printer"
  ([spec x] (explain* spec x nil))
  ([spec x opts]
   (explain* spec x opts)))
