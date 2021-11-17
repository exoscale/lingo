(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            exoscale.specs.string
            exoscale.specs.net
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [exoscale.specs :as xs]
            [clojure.walk :as walk]
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

(defn error-message
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

(def ^:dynamic *pred-matchers*
  '[[(contains? % ?key)
     (format "missing key %s" ?key)]

    [(meander.epsilon/pred set? ?set)
     (format "should be one of %s" (clojure.string/join "," (sort ?set)))]

    [(meander.epsilon/pred ident? ?id)
     (format "should match %s" (or (exoscale.lingo/spec-name ?id)
                                   (exoscale.lingo/strip-core ?id)))]

    ;; `every` (coll-of, etc...) :min-count, :max-count, :count

    [(<= ?min-count (count %) Integer/MAX_VALUE)
     (format "should contain at least %s elements"
             ?min-count)]

    [(<= 0 (count %) ?max-count)
     (format "should contain at most %s elements"
             ?max-count)]

    [(<= ?min-count (count %) ?max-count)
     (format "should contain between %s %s elements"
             ?min-count ?max-count)]

    [(meander.epsilon/or
      (= 1 (count %))
      (= (count %) 1))
     (format "should contain exactly 1 element")]

    [(meander.epsilon/or
      (= ?count (count %))
      (= (count %) ?count))
     (format "should contain exactly %s elements"
             ?count)]

    [(> (count %) ?count)
     (format "should contain more than %s elements"
             ?count)]

    [(< (count %) ?count)
     (format "should contain less than %s elements"
             ?count)]

    [(>= (count %) ?count)
     (format "should contain at least %s elements"
             ?count)]

    [(<= (count %) ?count)
     (format "should contain at most %s elements"
             ?count)]

    ;; int-in
    [(clojure.spec.alpha/int-in-range? ?min ?max %)
     (format "should be an Integer between %s %s"
             ?min ?max)]

    ;; double-in and other are bound be these
    [(<= % ?n) (format "should be smaller or equal than %d" ?n)]
    [(< % ?n) (format "should be smaller than %d" ?n)]

    [(>= % ?n) (format "should be greater or equal than %d" ?n)]
    [(> % ?n) (format "should be greater than %d" ?n)]

    [(meander.epsilon/or (= % ?x) (= ?x %)) (format "should be equal to %s" ?x)]
    [(meander.epsilon/or (not= ?x %) (not= % ?x)) (format "should not be equal to %s" ?x)]

    ;; double
    [(not (Double/isNaN %)) "cannot be NaN"]
    [(not (Double/isInfinite %)) "cannot be Infinite"]

    ;; exo specs
    [(exoscale.specs.string/string-of* % ?pt)
     (with-out-str
       (print "should be a String ")
       (let [{:keys [length min-length max-length blank? rx]} ?pt]
         (print (clojure.string/join ", "
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
                                       (conj (format "matching the regex %s" rx)))))))]

    [(.isValidInet4Address exoscale.specs.net/validator %)
     "Incorrect IPV4"]

    [(.isValidInet6Address exoscale.specs.net/validator %)
     "Incorrect IPV6"]])

(defn make-pred-matcher [ptns]
  ;; we could try to do this via a macro instead of using eval
  ;; to be revisited once/if we want to use this with cljs
  (eval `(fn [x#]
           (try
             (m/match x# ~@(apply concat ptns))
             (catch clojure.lang.ExceptionInfo e#
               (when-not (= (ex-message e#)
                            "non exhaustive pattern match")
                 (throw e#))
               x#)))))

(def ^:dynamic *pred-matcher*
  (make-pred-matcher *pred-matchers*))

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

(defn pred-str
  [pred pred-matcher]
  (pred-matcher (abbrev pred)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro def-pred-matcher
  [ptn-in ptn-out]
  `(let [ptn-in# ~ptn-in
         ptn-out# ~ptn-out
         ptns# (alter-var-root #'*pred-matchers*
                               (fn [pred-matchers#]
                                 (into [[ptn-in# ptn-out#]]
                                       (remove #(->> % first (= ptn-in#)))
                                       pred-matchers#)))]
     (alter-var-root #'*pred-matcher*
                     (fn [_#] (make-pred-matcher ptns#)))))

(defn explain-printer
  "Custom printer for explain-data. nil indicates a successful validation."
  [{:as ed
    :clojure.spec.alpha/keys [problems _spec value]
    :exoscale.lingo/keys [pred-matcher]
    :or {pred-matcher *pred-matcher*}}]
  (if ed
    (let [problems (->> problems

                        (sort-by #(- (count (:path %)))))]
      (doseq [{:keys [pred val reason via in _spec _path] :as prob} problems
              :let [err-message-override (some-> via last error-message)
                    spec (last via)]]

        (print (pr-str val))

        (when-not (empty? in)
          (print (format " in `%s`" (pr-str in))))

        (if spec
          (print (format " is an invalid %s" (spec-str spec)))
          (print " is invalid"))

        (print " - ")
        (cond
          (some? err-message-override)
          (print err-message-override)

          reason
          (print reason)

          :else
          (print (pred-str pred pred-matcher)))

        ;; (when-not (empty? path)
        ;;   (print (str " at: " (pr-str path))))

        ;; (when-not (empty? via)
        ;;   (let [spec (last via)]
        ;;     (print (spec-str spec))))
        (newline)

        (doseq [[k v] prob]
          (when-not (#{:path :pred :val :reason :via :in} k)
            (print "\n\t" (pr-str k) " ")
            (pr v)))))

    (println "Success!")))

(defn explain-data
  ([spec value]
   (explain-data spec value nil))
  ([spec value {:as _opts
                :exoscale.lingo/keys [pred-matcher]
                :or {pred-matcher *pred-matcher*}}]
   (-> (s/explain-data spec value)
       (update :clojure.spec.alpha/problems
               (fn [pbs]
                 (map (fn [{:keys [pred _val _reason via _in _spec _path] :as pb}]
                        (assoc pb :message
                               (or (some-> via last error-message)
                                   (pred-str pred pred-matcher))))
                      pbs))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn explain-str
  "Like spec explain-str, but uses lingo printer"
  [spec x]
  (with-out-str
    (binding [s/*explain-out* explain-printer]
      (s/explain spec x))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn explain
  "Like spec explain, but uses lingo printer"
  [spec x]
  (binding [s/*explain-out* explain-printer]
    (s/explain spec x)))

;; (s/def ::foo (s/coll-of string?))
;; (explain-data ::foo 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (comment


;; (defn space
;;   []
;;   (println)
;;   (println)
;;   (println (apply str (repeat 80 "-")))
;;   (println))


;; (do
;;   (space)
;;   (explain #{:a :b :c} "b"))

;; (do
;;   (space)
;;   ;; (def-pred-matcher! '(pos? (count %)) "should be non blank")
;;   (explain (s/and string? #(pos? (count %))) ""))

;; (do
;;   (space)
;;   (def-pred-matcher '(pos? (count %)) "should be non blank")
;;   (explain (s/and string? #(pos? (count %))) ""))

;; (require '[exoscale.specs.string :as xss])
;; (do
;;   (space)
;;   (explain (s/and string? #(xss/string-of* % {:blank? false :min-length 3 :max-length 10})) ""))

;; ;;   (space)
;; ;;   (explain (s/and string? something?) ""))

;; (do
;;   (space)
;;   (explain zero? 1))

;; (do
;;   (space)
;;   (s/def :exoscale.lingo/c1 (s/map-of int? int? :count 3))
;;   (explain :exoscale.lingo/c1 {"a" "b"}))

;; (do
;;   (space)
;;   (s/def :exoscale.lingo/c1 neg-int?)
;;   (explain :exoscale.lingo/c1 [1 1]))

;; (do
;;   (s/def :foo/agent (s/coll-of (s/keys :req-un [:foo/person :foo/age])))
;;   (explain :foo/agent [{:age 10}]))

;; (s/def :foo/age #(< % 30))
;; (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
;; (explain :foo/agent {:age 100, :person {:names '(1)}})

;; (do
;;   (space)
;;   (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
;;   (explain :foo/agent {:age 10 :person {:names [1]}}))

;; (do
;;   (space)
;;   (explain :foo/agent2 {:age ""}))

;; (do
;;   (space)
;;   (s/def :foo/animal #{:a :b :c})
;;   (explain :foo/animal 1))

;; (do
;;   (space)
;;   (explain-str :foo/person {:names [1 :yolo]}))

;; (do
;;   (explain nil? 1)))

;; (explain (s/coll-of any? :max-count 3 :min-count 2) [1 2 3 4])
;; (require '[exoscale.specs.string :as xss])

;; (do
;;   (space)
;;   (explain (xss/string-of {:blank? true}) ""))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal (s/coll-of :exoscale.lingo/names))
;;   (println (explain-str :exoscale.lingo/animal [[:exoscale.lingo/foo :bar]]))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal #{:a :b :c})
;;   (println (explain-str :exoscale.lingo/animal 1))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal #{:a :b :c})
;;   (println (explain-str :exoscale.lingo/animal "d"))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal2 :exoscale.lingo/animal)
;;   (println (explain-str :exoscale.lingo/animal2 1))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal2 :exoscale.lingo/animal)
;;   (println (explain-str :exoscale.lingo/animal2 "a"))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def :exoscale.lingo/animal2 :exoscale.lingo/animal)
;;   (println (explain-str int? "1"))
;;   (println))
;; (explain :exoscale.specs.net/url "")

;; (defn f2?
;;   [x]
;;   false)

;; (xs/with-meta! `f2? {::name "yolo"})

;; ;; (def-pred-matcher 'exoscale.lingo/f2? "boom")

;; (explain f2? 1)
