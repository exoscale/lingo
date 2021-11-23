(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            [exoscale.lingo.impl :as impl]
            [clojure.string :as str]))

(def registry-ref (atom #:exoscale.lingo{:specs {} :preds {}}))

(defn set-pred-error!
  "Set conforming spec `spec-ptn` for matching/binding values for later
  message formating via `f bindings`"
  [spec-ptn f]
  (swap! registry-ref
         assoc-in
         [:exoscale.lingo/preds spec-ptn]
         f))

(defn set-spec-error!
  "Set error message for `spec` (keyword, ident, s-expr (pred)) with `msg`"
  [spec msg]
  (swap! registry-ref
         assoc-in
         [:exoscale.lingo/specs spec]
         msg))

(def default-opts
  #:exoscale.lingo{:registry registry-ref
                   ;; use (memoize s/conform) for fast lookup
                   :conform s/conform})

(defn explain-data
  ([spec value]
   (explain-data spec value nil))
  ([spec value opts]
   (let [opts (into default-opts opts)]
     (some-> (s/explain-data spec value)
             (update :clojure.spec.alpha/problems
                     (fn [pbs]
                       (map (fn [{:keys [pred _val _reason via in _spec _path] :as pb}]
                              (let [spec (last via)
                                    path (impl/path-str in)]
                                (cond-> (assoc pb
                                               :exoscale.lingo/message
                                               (or
                                                ;; first try to find a custom error for this specific
                                                ;; `spec` key (and potential aliases)
                                                (impl/find-ident-error-message spec opts)
                                                ;; try to find message for `pred` via custom error
                                                ;; first and then using the matcher if that fails
                                                (impl/find-pred-error-message pred opts)
                                                ;; all failed, return abreviated pred form
                                                (impl/abbrev pred)))
                                  path
                                  (assoc :exoscale.lingo/path path))))
                            (sort-by #(- (count (:path %)))
                                     pbs))))))))

(defn explain
  "Like spec explain, but uses lingo printer"
  ([spec value] (explain spec value nil))
  ([spec value opts]
   (if-let [{:as _ed :clojure.spec.alpha/keys [problems]} (explain-data spec value opts)]
     (doseq [{:as _problem
              :exoscale.lingo/keys [message]
              :keys [via in val]} problems
             :let [spec (last via)]]
       (print (pr-str val))

       (when-not (empty? in)
         (print (format " in `%s`" (impl/path-str in))))

       (if spec
         (print (format " is an invalid %s" (pr-str spec)))
         (print " is invalid"))

       (print " - ")
       (print message)
       (newline))

     (println "Success!"))))

(defn explain-str
  "Like spec explain-str, but uses lingo printer"
  ([spec x] (explain-str spec x nil))
  ([spec x opts]
   (with-out-str
     (explain spec x opts))))

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

;;; pred errors

(set-pred-error! set? #(format "should be one of %s" (str/join "," (sort %))))

(set-pred-error! (s/cat :pred #{'contains?}
                        :arg any?
                        :key any?)
                 #(format "missing key %s" (:key %)))

(set-pred-error! (s/cat :_op #{'<=}
                        :min number?
                        :_cnt #{'(count %)}
                        :_max #{'Integer/MAX_VALUE})
                 #(format "should contain at least %s elements"
                          (:min %)))

(set-pred-error! (s/cat :_op #{'>=}
                        :_zero #{0}
                        :_cnt #{'(count %)}
                        :max number?)
                 #(format "should contain at most %s elements"
                          (:max %)))

(set-pred-error! (s/cat :_op #{'<=}
                        :min number?
                        :_cnt #{'(count %)}
                        :max number?)
                 #(format "should contain between %s %s elements"
                          (:min %)
                          (:max %)))

(set-pred-error! (s/or :count-1
                       (s/cat :op #{'= '< '> '<= '>= 'not=}
                              :_ #{'(count %)}
                              :x any?)
                       :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                                       :x number?
                                       :_ #{'(count %)}))
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
                             "elements"))))

(set-pred-error! (s/or :count-1
                       (s/cat :op #{'= '< '> '<= '>= 'not=}
                              :_ #{'%}
                              :x any?)
                       :count-2 (s/cat :op #{'= '< '> '<= '>= 'not=}
                                       :x any?
                                       :_ #{'%}))
                 (fn [[_ {:keys [op x]}]]
                   (format "should %s %s"
                           (case op
                             not= "not be equal to"
                             = "be equal to"
                             > "be greater than"
                             < "be less than"
                             >= "be at least"
                             <= "be at most")
                           x)))

(set-pred-error! (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
                        :min number?
                        :max number?
                        :_ #{'%})
                 (fn [{:keys [min max]}]
                   (format "should be an Integer between %d %d" min max)))

#_(set-pred-explain! (s/cat :_ #{'exoscale.specs.string/string-of*}
                            :_ #{'%}
                            :opts map?)
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
                                                     (conj (format "matching the regex %s" rx))))))))
