(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [exoscale.lingo.highlight :as u]
            [exoscale.lingo.impl :as impl]
            [exoscale.lingo :as l]))

(def registry-ref (atom (merge #:exoscale.lingo.registry.spec{:message {}}
                               #:exoscale.lingo.registry.pred{:conformers #{}
                                                              :message {}})))

(defn set-pred-conformer!
  [k]
  (swap! registry-ref update :exoscale.lingo.registry.pred/conformers conj k))

(defn set-pred-message!
  [k f]
  (swap! registry-ref assoc-in [:exoscale.lingo.registry.pred/message k] f))

(defn set-pred-error!
  "Set conforming spec `spec-ptn` for matching/binding values for later
  message impl/formating via `f bindings`"
  [k f]
  (set-pred-conformer! k)
  (set-pred-message! k f))

(defn set-spec-error!
  "Set error message for `spec` (keyword, ident, s-expr (pred)) with `msg`"
  [spec msg]
  (swap! registry-ref
         assoc-in
         [:exoscale.lingo.registry.spec/message spec]
         msg))

(def default-opts
  {:registry registry-ref
   :conform (memoize s/conform)
   :header? true
   :focus? true
   :colors? false
   :path? true
   :message? true
   :highlight? true
   :highlight-inline-message? true})

(defn x-fix-spec-quirks
  [value]
  (map (fn [pb]
         ;; workaround for CLJ-2682 maybe we should also patch :path, but for
         ;; now we do not use it so we don't have to care
         (assoc pb :in (impl/fix-map-path value (:in pb))))))

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

(defn x-extend-message
  [{:as opts :keys [registry]}]
  (map (fn [pb]
         (let [spec-msg (:exoscale.lingo.explain.spec/message pb)
               pred-msg (when-let [vals (:exoscale.lingo.explain.pred/vals pb)]
                          (when-let [f (get-in @registry
                                               [:exoscale.lingo.registry.pred/message
                                                (:exoscale.lingo.explain.pred/spec pb)])]
                            (f vals opts)))
               msg (or spec-msg pred-msg)]
           (cond-> pb
             pred-msg (assoc :exoscale.lingo.explain.pred/message pred-msg)
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
           (assoc :exoscale.lingo.explain/highlight
                  (u/highlight val pb opts))))))

(defn explain-data*
  [{:as explain-data :clojure.spec.alpha/keys [value]}
   {:as opts :keys [highlight? group-missing-keys? path? message?]}]
  (update explain-data
          :clojure.spec.alpha/problems
          (fn [pbs]
            (sequence (comp
                       (if message? (x-extend-message opts) identity)
                       (if path? (x-extend-path opts) identity)
                       (if highlight? (x-highlight value opts) identity))
                      ;; the stuff we will always do first, fixing spec bugs,
                      ;; extending with our spec/pred data. We need to do this
                      ;; first because of grouping, this way we can avoid to do
                      ;; useless work that would be squashed by a potential
                      ;; merge of problems
                      (cond->> (eduction (x-fix-spec-quirks value)
                                         (x-extend-spec-data opts)
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

(defn explain-printer
  "Like spec explain, but uses lingo printer"
  ([ed] (explain-printer ed nil))
  ([{:as _ed :clojure.spec.alpha/keys [problems]}
    {:as _opts :keys [colors? highlight? header?]}]
   (if (seq problems)
     (do
       (when header?
         (println (str (count problems) " errors found"))
         (newline))
       (doseq [{:as _problem
               :exoscale.lingo.explain/keys [message highlight]
               :keys [via in val pred]} problems
              :let [spec (last via)]]
        (if (and highlight? highlight)
          (do
            (print (str "--> Invalid "))
            (print
             (cond-> (if spec
                       (pr-str spec)
                       "value")
               colors?
               (u/color :red)))

            ;; (newline)
            ;; (newline)
            (when-not (empty? in)
              (print (impl/format " in `%s`"
                                  (cond-> (impl/path-str in)
                                    colors?
                                    (u/color :cyan))))
              (newline))

            (let [fringe "  |  " ]
              (println fringe)
              (print (u/prefix-lines highlight fringe))
              (newline)
              (println fringe))

            (newline))
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
              (newline))))))

     (println "Success!"))))

(defn set-explain-printer!
  "Sets explain printer globally for all specs"
  ([] (set-explain-printer! nil))
  ([opts]
   (set! clojure.spec.alpha/*explain-out*
         (let [opts (into default-opts opts)]
           (fn [ed]
             (explain-printer (explain-data* ed opts)
                              opts))))))

(defn explain
  "Like spec explain, but uses lingo printer"
  ([spec value] (explain spec value nil))
  ([spec value opts]
   (let [opts (into default-opts opts)]
     (explain-printer (explain-data spec value opts) opts))))

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
(set-spec-error! `list? "should be a List")
(set-spec-error! `seq? "should be a Seq")
(set-spec-error! `rational? "should be a Rational Number")
(set-spec-error! `ratio? "should be a Ratio")
(set-spec-error! `vector? "should be a Vector")
(set-spec-error! `associative? "should be an Associative (map, vector)")
(set-spec-error! `sequential?  "should be a Sequential")
(set-spec-error! `seqable?  "should be a Seqable")
(set-spec-error! `indexed?  "should be an Indexed")
(set-spec-error! `number? "should be a Number")
(set-spec-error! `bytes? "should be byte array")
(set-spec-error! `float? "should be a Float")
(set-spec-error! `double? "should be a Double")
(set-spec-error! `boolean? "should be a Boolean")
(set-spec-error! `true? "should be true")
(set-spec-error! `false? "should be false")
(set-spec-error! `zero? "should be Zero")
(set-spec-error! `empty? "should be Empty")
(set-spec-error! `ident? "should be an Identifier (keyword or symbol)")
(set-spec-error! `ident? "should be a simple Identifier (no namespace)")
(set-spec-error! `qualified-ident? "should be a namespace qualified Identifier")
(set-spec-error! `keyword? "should be an Keyword")
(set-spec-error! `simple-keyword? "should be a simple keyword (no namespace)")
(set-spec-error! `qualified-keyword? "should be a namespace qualified Keyword")
(set-spec-error! `symbol? "should be a Symbol")
(set-spec-error! `simple-symbol? "should be a simple Symbol (no namespace)")
(set-spec-error! `qualified-symbol? "should be a namespace qualified Symbol")
(set-spec-error! `uuid? "should be a UUID")
(set-spec-error! `uri? "should be a URI")
(set-spec-error! `decimal? "should be a decimal")
(set-spec-error! `int? "should be an Integer")
(set-spec-error! `integer? "should be an Integer")
(set-spec-error! `nat-int? "should be an Integer")
(set-spec-error! `pos-int? "should be a Positive Integer")
(set-spec-error! `neg-int? "should be a Negative Integer")
(set-spec-error! `pos? "should be a Positive number")
(set-spec-error! `neg? "should be a Negative number")
(set-spec-error! `inst? "should be a Instant")
(set-spec-error! `some? "should be Non-nil")
(set-spec-error! `nil? "should be nil")

(set-spec-error! `some? "should be non-nil")

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

(set-pred-message! :exoscale.lingo.pred/contains-keys
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

(comment
  (s/def :foo/t-shirts (s/coll-of :foo/t-shirt))
  (s/def :foo/t-shirt (s/keys :req-un [:foo/size :foo/color]))
  (s/def :foo/size (s/int-in 1 3))
  (s/def :foo/color #{:red :blue :green})

  (exoscale.lingo/explain :foo/t-shirts [{:size 5 :color :pink}] {:colors? true}))
