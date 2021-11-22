(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            exoscale.specs.string
            exoscale.specs.net
            [exoscale.specs :as xs]
            [clojure.walk :as walk]
            [clojure.string :as str]))

(defn with-error!
  "Adds custom error message for a spec"
  [spec error-msg]
  (xs/vary-meta! spec
                 assoc :exoscale.lingo/error error-msg))

;; set defaults for common idents
(with-error! `string? "should be a String")
(with-error! `char? "should be a Character")
(with-error! `map? "should be a Map")
(with-error! `coll? "should be a Collection")
(with-error! `set? "should be a Set")
(with-error! `vector? "should be a Vector")
(with-error! `associative? "should be an Associative (map, vector)")
(with-error! `sequential?  "should be a Sequential")
(with-error! `number? "should be a Number")
(with-error! `bytes? "should be a Bytes")
(with-error! `float? "should be a Float")
(with-error! `double? "should be a Double")
(with-error! `boolean? "should be a Boolean")
(with-error! `true? "should be true")
(with-error! `false? "should be false")
(with-error! `zero? "should be Zero")
(with-error! `empty? "should be Empty")
(with-error! `ident? "should be an Identifier (keyword or symbol)")
(with-error! `qualified-ident? "should be a Qualified Identifier (keyword or symbol)")
(with-error! `symbol? "should be a Symbol")
(with-error! `uuid? "should be a UUID")
(with-error! `uri? "should be a URI")
(with-error! `int? "should be an Integer")
(with-error! `nat-int? "should be an Integer")
(with-error! `pos-int? "should be a Positive Integer")
(with-error! `neg-int? "should be a Negative Integer")
(with-error! `pos? "should be a Positive number")
(with-error! `neg? "should be a Negative number")
(with-error! `inst? "should be a Instant")
(with-error! `some? "should be Non-nil")
(with-error! `nil? "should be nil")

(defn spec-error-message
  [spec]
  (get (xs/meta spec) :exoscale.lingo/error))

(defn spec-str
  [spec]
  (pr-str spec))

(defn strip-core
  [sym]
  (cond-> sym
    (= (namespace sym) "clojure.core")
    (-> name symbol)))

(def matchers-registry (atom {}))

(defn register-matcher!
  ([spec-key f]
   (register-matcher! matchers-registry spec-key f))
  ([registry spec-key f]
   (swap! registry assoc spec-key f)))

(-> (s/def ::set set?)
    (register-matcher!
     #(format "should be one of %s" (clojure.string/join "," (sort %)))))

(-> (s/def ::contains-key
      (s/cat :pred #{'contains?}
             :arg any?
             :key any?))
    (register-matcher! #(format "missing key %s" (:key %))))

(-> (s/def ::min-count
      (s/cat :_op #{'<=}
             :min number?
             :_cnt #{'(count %)}
             :_max #{'Integer/MAX_VALUE}))
    (register-matcher!
     #(format "should contain at least %s elements"
              (:min %))))

(-> (s/def ::max-count
      (s/cat :_op #{'>=}
             :_zero zero?
             :_cnt #{'(count %)}
             :max number?))
    (register-matcher!
     #(format "should contain at most %s elements"
              (:max %))))

(-> (s/def ::between-count
      (s/cat :_op #{'<=}
             :min number?
             :_cnt #{'(count %)}
             :max number?))
    (register-matcher!
     #(format "should contain between %s %s elements"
              (:min %)
              (:max %))))

(-> (s/def ::compare-count-op
      (s/or :count-1
            (s/cat :op #{'= '< '> '<= '>= 'not=}
                   :_ #{'(count %)}
                   :x any?)
            :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                            :x number?
                            :_ #{'(count %)})))
    (register-matcher!
     (fn [[_ {:keys [op x]}]]
       (format "should contain %s %s %s"
               (case op
                 not= "not ="
                 = "exactly"
                 > "more than"
                 < "less than"
                 >= "at least"
                 <= "at most")
               x
               (if (= 1 x)
                 "element"
                 "elements")))))

(-> (s/def ::compare-op
      (s/or :count-1
            (s/cat :op #{'= '< '> '<= '>= 'not=}
                   :_ #{'%}
                   :x any?)
            :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                            :x any?
                            :_ #{'%})))
    (register-matcher!
     (fn [[_ {:keys [op x]}]]
       (format "should %s %s"
               (case op
                 not= "not be equal to"
                 = "be equal to"
                 > "be greater than"
                 < "be less than"
                 >= "be at least"
                 <= "be at most")
               x))))

(-> (s/def ::int-in
      (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
             :min number?
             :max number?
             :_ #{'%}))
    (register-matcher!
     (fn [{:keys [min max]}]
       (format "should be an Integer between %d %d" min max))))

(-> (s/def ::string-of (s/cat :_ #{'exoscale.specs.string/string-of*}
                              :_ #{'%}
                              :opts map?))
    (register-matcher!
     (fn [{:keys [opts]}]
       (let [{:keys [length min-length max-length blank? rx]} opts]
         (str "should be a String "
              (clojure.string/join ", "
                                   (cond-> []
                                     (false? blank?)
                                     (conj "non blank")
                                     min-length
                                     (conj (format "at least %d characters in length" min-length))
                                     max-length
                                     (conj (format "at most %d characters in length" max-length))
                                     length
                                     (conj (format "exactly %d characters in length" length))
                                     rx
                                     (conj (format "matching the regex %s" rx)))))))))

(defn find-registry-pred-message
  [registry x]
  (reduce (fn [_ [k formater]]
            (when-let [match (and (s/get-spec k) (s/conform k x))]
              (when (not= match :clojure.spec.alpha/invalid)
                (reduced (formater match)))))
          nil
          @registry))

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

(defn find-pred-error-message
  [pred registry]
  (if (qualified-ident? pred)
    (spec-error-message (abbrev pred))
    (find-registry-pred-message registry
                                (abbrev pred))))

(defn find-ident-error-message
  [spec]
  (reduce (fn [_ spec]
            (when-let [msg (spec-error-message (abbrev spec))]
              (reduced msg)))
          nil
          (spec-vals spec)))

(defn path-str
  [in]
  (when (seq in)
    (letfn [(mdot [s] (when (seq s) (str ".")))]
      (reduce (fn [s segment]
                (str s
                     (cond
                       (nat-int? segment)
                       (format "[%s]" segment)

                       (keyword? segment)
                       (str (mdot s) (name segment))

                       :else
                       (str (mdot s) (str segment)))))
              nil
              in))))

(defn explain-data
  ([spec value]
   (explain-data spec value nil))
  ([spec value {:as _opts
                :exoscale.lingo/keys [registry]
                :or {registry matchers-registry}}]
   (some-> (s/explain-data spec value)
           (update :clojure.spec.alpha/problems
                   (fn [pbs]
                     (map (fn [{:keys [pred _val _reason via in _spec _path] :as pb}]
                            (let [spec (last via)
                                  path (path-str in)]
                              (cond-> (assoc pb
                                             :exoscale.lingo/message
                                             (or
                                              ;; first try to find a custom error for this specific
                                              ;; `spec` key (and potential aliases)
                                              (find-ident-error-message spec)
                                              ;; try to find message for `pred` via custom error
                                              ;; first and then using the matcher if that fails
                                              (find-pred-error-message pred registry)
                                              ;; all failed, return abreviated pred form
                                              (abbrev pred)))
                                path
                                (assoc :exoscale.lingo/path path))))
                          (sort-by #(- (count (:path %)))
                                   pbs)))))))


(defn explain*
  [spec value opts]
  (if-let [{:as _ed :clojure.spec.alpha/keys [problems]} (explain-data spec value opts)]
    (doseq [{:as _problem
             :exoscale.lingo/keys [message]
             :keys [via in val]} problems
            :let [spec (last via)]]
      (print (pr-str val))

      (when-not (empty? in)
        (print (format " in `%s`" (path-str in))))

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
