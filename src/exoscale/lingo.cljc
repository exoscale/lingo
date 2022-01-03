(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [exoscale.lingo.highlight :as u]
            [exoscale.lingo.impl :as impl]
            [exoscale.lingo :as l]))

(def registry-ref (atom (merge #:exoscale.lingo.registry.spec{:msg {}}
                               #:exoscale.lingo.registry.pred{:conformers #{}
                                                              :msg {}})))

(defn set-pred-conformer!
  [k]
  (swap! registry-ref update :exoscale.lingo.registry.pred/conformers conj k))

(defn set-pred-msg!
  [k f]
  (swap! registry-ref assoc-in [:exoscale.lingo.registry.pred/msg k] f))

(defn set-pred-error!
  "Set conforming spec `spec-ptn` for matching/binding values for later
  message impl/formating via `f bindings`"
  [k f]
  (set-pred-conformer! k)
  (set-pred-msg! k f))

(defn set-spec-error!
  "Set error message for `spec` (keyword, ident, s-expr (pred)) with `msg`"
  [spec msg]
  (swap! registry-ref
         assoc-in
         [:exoscale.lingo.registry.spec/msg spec]
         msg))

(def default-opts
  {:registry registry-ref
   ;; use (memoize s/conform) for fast lookup
   :conform s/conform
   :highlight? true
   :highlight-inline-message? false
   :highlight-colors? false})

(defn x-extend-pred-data
  [opts]
  (map (fn [{:keys [pred] :as pb}]
         (let [pred-data (impl/find-pred-data pred opts)]
           (cond-> pb
             pred-data (into pred-data))))))

(defn x-extend-spec-data
  [opts]
  (map (fn [{:keys [via] :as pb}]
         (let [ident-data (impl/find-spec-data (last via) opts)]
           (cond-> pb
             ident-data (into ident-data))))))

(defn x-extend-msg
  [{:as opts :keys [registry]}]
  (map (fn [pb]
         (let [spec-msg (:exoscale.lingo.explain.spec/msg pb)
               pred-msg (when-let [vals (:exoscale.lingo.explain.pred/vals pb)]
                          (when-let [f (get-in @registry
                                               [:exoscale.lingo.registry.pred/msg
                                                (:exoscale.lingo.explain.pred/spec pb)])]
                            (f vals opts)))
               msg (or spec-msg pred-msg)]
           (cond-> pb
             pred-msg (assoc :exoscale.lingo.explain.pred/msg pred-msg)
             msg (assoc :exoscale.lingo.explain/message msg))))))

(defn x-extend-path
  [_opts]
  (map (fn [{:keys [in] :as pb}]
         (let [path (impl/path-str in)]
           (cond-> pb
             path (assoc :exoscale.lingo.explain/path path))))))

(defn x-highlight
  [val opts]
  (map (fn [{:keys [in] :as pb}]
         (cond-> pb
           (seq in)
           (assoc :exoscale.lingo.explain/highlight (u/highlight val pb opts))))))

(defn explain-data*
  [{:as explain-data :clojure.spec.alpha/keys [value]}
   {:as opts :keys [highlight? group-missing-keys?]}]
  (update explain-data
          :clojure.spec.alpha/problems
          (fn [pbs]
            (sequence (comp
                       (x-extend-msg opts)
                       (x-extend-path opts)
                       (if highlight?
                         (x-highlight value opts)
                         identity))
                      (cond->> (eduction (x-extend-spec-data opts)
                                         (x-extend-pred-data opts)
                                         pbs)
                        group-missing-keys?
                        impl/group-missing-keys
                        :then (sort-by #(- (count (:path %)))))))))

(defn explain-data
  ([spec value]
   (explain-data spec value nil))
  ([spec value opts]
   (some-> (s/explain-data spec value)
           (explain-data* (into default-opts opts)))))

(defn explain
  "Like spec explain, but uses lingo printer"
  ([spec value] (explain spec value nil))
  ([spec value opts]
   (let [{:as opts :keys [highlight? highlight-inline-message?]}
         (into default-opts opts)]
     (if-let [{:as _ed
               :clojure.spec.alpha/keys [problems]} (explain-data spec value opts)]
       (doseq [{:as _problem
                :exoscale.lingo.explain/keys [message highlight]
                :keys [via in val pred]} problems
               :let [spec (last via)]]
         (if highlight-inline-message?
           (do
             (if spec
               (print (impl/format "Invalid %s" (pr-str spec)))
               (print "Invalid value"))

             (when-not (empty? in)
               (print (impl/format " in `%s`" (impl/path-str in))))

             (when (and highlight? highlight)
               (newline)
               (newline)
               (print highlight)
               (newline)))
           (do
             (print (pr-str val))

             (when-not (empty? in)
               (print (impl/format " in `%s`" (impl/path-str in))))

             (if spec
               (print (impl/format " is an invalid %s" (pr-str spec)))
               (print " is invalid"))

             (print " - ")
             (print (or message (impl/abbrev pred)))
             (newline)

             (when (and highlight? highlight)
               (newline)
               (print highlight)
               (newline)))))

       (println "Success!")))))

(defn explain-str
  "Like spec explain-str, but uses lingo printer"
  ([spec x] (explain-str spec x nil))
  ([spec x opts]
   (with-out-str
     (explain spec x (into default-opts opts)))))

;;; Set defaults for common specs and preds

(set-spec-error! `string? "should be a String")
(set-spec-error! `char? "should be a Character")
(set-spec-error! `map? "should be a Map")
(set-spec-error! `coll? "should be a Collection")
(set-spec-error! `set? "should be a Set")
(set-spec-error! `vector? "should be a Vector")
(set-spec-error! `associative? "should be an Associative (map, vector)")
(set-spec-error! `sequential?  "should be a Sequential")
(set-spec-error! `number? "should be a Number")
(set-spec-error! `bytes? "should be a Bytes")
(set-spec-error! `float? "should be a Float")
(set-spec-error! `double? "should be a Double")
(set-spec-error! `boolean? "should be a Boolean")
(set-spec-error! `true? "should be true")
(set-spec-error! `false? "should be false")
(set-spec-error! `zero? "should be Zero")
(set-spec-error! `empty? "should be Empty")
(set-spec-error! `ident? "should be an Identifier (keyword or symbol)")
(set-spec-error! `qualified-ident? "should be a Qualified Identifier (keyword or symbol)")
(set-spec-error! `symbol? "should be a Symbol")
(set-spec-error! `uuid? "should be a UUID")
(set-spec-error! `uri? "should be a URI")
(set-spec-error! `int? "should be an Integer")
(set-spec-error! `nat-int? "should be an Integer")
(set-spec-error! `pos-int? "should be a Positive Integer")
(set-spec-error! `neg-int? "should be a Negative Integer")
(set-spec-error! `pos? "should be a Positive number")
(set-spec-error! `neg? "should be a Negative number")
(set-spec-error! `inst? "should be a Instant")
(set-spec-error! `some? "should be Non-nil")
(set-spec-error! `nil? "should be nil")

;; pred errors
(set-pred-error! (s/def :exoscale.lingo.pred/symbol symbol?)
                 (fn [sym {:as _opts :keys [registry]}]
                   (impl/spec-error-message (if (simple-symbol? sym)
                                              (symbol "clojure.core" (name sym))
                                              sym)
                                            @registry)))

(set-pred-error! (s/def :exoscale.lingo.pred/set set?)
                 (fn [st _opts]
                   (impl/format "should be one of %s" (str/join ", " (sort st)))))

(set-pred-error! (s/def :exoscale.lingo.pred/contains-key
                   (s/cat :pred #{'contains?}
                          :arg #{'%}
                          :key keyword?))
                 (fn [{:keys [key]} opts]
                   (impl/format "missing key %s"
                                (cond-> key
                                  (:hide-keyword-namespaces? opts)
                                  (-> name keyword)))))

(set-pred-msg! :exoscale.lingo.pred/contains-keys
               (fn [{:keys [keys]} opts]
                 (impl/format "missing keys %s"
                              (->> keys
                                   (map #(cond-> %
                                           (:hide-keyword-namespaces? opts)
                                           (-> name keyword)))
                                   (str/join ", ")))))

(s/def ::count+arg (s/spec (s/cat :_ #{'count} :sym simple-symbol?)))

(set-pred-error! (s/def :exoscale.lingo.pred/gte-count
                   (s/cat :_op #{'<=}
                          :min number?
                          :_cnt ::count+arg
                          :_max #{'Integer/MAX_VALUE}))
                 (fn [{:keys [min]} _opts]
                   (impl/format "should contain at least %s elements"
                                min)))

(set-pred-error! (s/def :exoscale.lingo.pred/lte-count
                   (s/cat :_op #{'>=}
                          :_zero #{0}
                          :_cnt ::count+arg
                          :max number?))
                 (fn [{:keys [max _opts]}]
                   (impl/format "should contain at most %s elements"
                                max)))

(set-pred-error! (s/def :exoscale.lingo.pred/between-count
                   (s/cat :_op #{'<=}
                          :min number?
                          :_cnt ::count+arg
                          :max number?))
                 (fn [{:keys [min max]} _opts]
                   (impl/format "should contain between %s %s elements"
                                min max)))

(set-pred-error! (s/def :exoscale.lingo.pred/compare-count
                   (s/or :count-1
                         (s/cat :op #{'= '< '> '<= '>= 'not=}
                                :_ ::count+arg
                                :x any?)
                         :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                                         :x number?
                                         :_ ::count+arg)))
                 (fn [[_ {:keys [op x]}] _opts]
                   (impl/format "should contain %s %s %s"
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
                                  "elements"))))

(set-pred-error! (s/def :exoscale.lingo.pred/num-compare
                   (s/or :count-1
                         (s/cat :op #{'= '< '> '<= '>= 'not=}
                                :_ simple-symbol?
                                :x any?)
                         :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                                         :x any?
                                         :_ simple-symbol?)))
                 (fn [[_ {:keys [op x]}] _opts]
                   (impl/format "should %s %s"
                                (case op
                                  not= "not be equal to"
                                  = "be equal to"
                                  > "be greater than"
                                  < "be less than"
                                  >= "be at least"
                                  <= "be at most")
                                x)))

(set-pred-error! (s/def :exoscale.lingo.pred/int-in-range
                   (s/or :_ (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
                                   :min number?
                                   :max number?
                                   :_ simple-symbol?)
                         :_ (s/cat :_ #{'<=}
                                   :min number?
                                   :_ simple-symbol?
                                   :max number?)))
                 (fn [[_ {:keys [min max]}] _opts]
                   (impl/format "should be an Integer between %d %d" min max)))
