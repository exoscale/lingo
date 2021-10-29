(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            [exoscale.specs :as xs]
            [clojure.walk :as walk]
            clojure.string
            [meander.epsilon :as m]))

;; set defaults for common idents

(xs/with-meta! `string? {::name "String"})
(xs/with-meta! `char? {::name "Character"})
(xs/with-meta! `map? {::name "Map"})
(xs/with-meta! `coll? {::name "Collection"})
(xs/with-meta! `set? {::name "Set"})
(xs/with-meta! `vector? {::name "Vector"})
(xs/with-meta! `associative? {::name "Associative (map, vector)"})
(xs/with-meta! `sequential? {::name "Sequential"})
(xs/with-meta! `number? {::name "Number"})
(xs/with-meta! `bytes? {::name "Bytes"})
(xs/with-meta! `float? {::name "Float"})
(xs/with-meta! `double? {::name "Double"})
(xs/with-meta! `boolean? {::name "Boolean"})
(xs/with-meta! `true? {::name "true"})
(xs/with-meta! `false? {::name "false"})
(xs/with-meta! `zero? {::name "Zero"})
(xs/with-meta! `empty? {::name "Empty"})
(xs/with-meta! `ident? {::name "Identifier (keyword or symbol)"})
(xs/with-meta! `qualified-ident? {::name "Qualified Identifier (keyword or symbol)"})
(xs/with-meta! `symbol? {::name "Symbol"})
(xs/with-meta! `uuid? {::name "UUID"})
(xs/with-meta! `uri? {::name "URI"})
(xs/with-meta! `int? {::name "Integer"})
(xs/with-meta! `nat-int? {::name "Integer"})
(xs/with-meta! `pos-int? {::name "Positive Integer"})
(xs/with-meta! `neg-int? {::name "Negative Integer"})
(xs/with-meta! `inst? {::name "Instant"})
(xs/with-meta! `some? {::name "Non-nil"})
(xs/with-meta! `nil? {::name "nil"})

(defn spec-name
  [spec]
  (get (xs/meta spec) ::name))

(defn spec-str
  [spec]
  (if-let [sn (spec-name spec)]
    (format " - spec: %s (%s)" (pr-str spec) sn)
    (str " - spec: " (pr-str spec))))

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

    [(= 1 (count %))
     (format "should contain only 1 element")]

    [(= ?count (count %))
     (format "should contain exactly %s elements"
             ?count)]

    [(> (count %) ?count)
     (format "should contain more than %s elements"
             ?count)]

    [(< (count %) ?count)
     (format "should contain less than %s elements"
             ?count)]

    [(>= (count %) ?count)
     (format "should contain more or exactly %s elements"
             ?count)]

    [(<= (count %) ?count)
     (format "should contain less or exaclty %s elements"
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

    [(= % ?x) (format "should be equal to %s" ?x)]
    [(not= % ?x) (format "should not be equal to %s" ?x)]

    ;; double
    [(not (Double/isNaN %)) "cannot be NaN"]
    [(not (Double/isInfinite %)) "cannot be Infinite"]

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
                                       (conj (format "matching the regex %s" rx)))))))]])

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

                         ;; ;; not core
                         ;; (and qs? (not core?))
                         ;; form

                         (and (seq? form)
                              (= 'fn (first form))
                              ;; (= '[%] (second form))
                              )
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
                                 (-> (empty pred-matchers#)
                                     (into (remove #(->> % first (= ptn-in#)))
                                           pred-matchers#)
                                     (conj [ptn-in# ptn-out#]))))]
     (alter-var-root #'*pred-matcher*
                     (fn [_#] (make-pred-matcher ptns#)))))

(defn explain-printer
  "Custom printer for explain-data. nil indicates a successful validation."
  [{:as ed
    :clojure.spec.alpha/keys [problems]
    ::keys [pred-matcher]
    :or {pred-matcher *pred-matcher*}}]
  (if ed
    (let [problems (->> problems
                        (sort-by #(- (count (:in %))))
                        (sort-by #(- (count (:path %)))))]
      (doseq [{:keys [_path pred val reason via in] :as prob} problems]
        (print (pr-str val))
        (print " is invalid: ")
        (if reason
          (print reason)
          (print (pred-str pred pred-matcher)))

        (when-not (empty? in)
          (print (str " in: " (pr-str in))))

        ;; (when-not (empty? path)
        ;;   (print (str " at: " (pr-str path))))

        (when-not (empty? via)
          (let [spec (last via)]
            (print (spec-str spec))))

        (doseq [[k v] prob]
          (when-not (#{:path :pred :val :reason :via :in} k)
            (print "\n\t" (pr-str k) " ")
            (pr v)))

        (newline)))

    (println "Success!")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (defn space
    []
    (println)
    (println)
    (println (apply str (repeat 80 "-")))
    (println))


  ;; (do
  ;;   (space)
  ;;   (explain #{:a :b :c} "b"))

  (do
    (space)
    ;; (def-pred-matcher! '(pos? (count %)) "should be non blank")
    (explain (s/and string? #(pos? (count %))) ""))

  (do
    (space)
    (def-pred-matcher '(pos? (count %)) "should be non blank")
    (explain (s/and string? #(pos? (count %))) ""))

  (require '[exoscale.specs.string :as xss])
  (do
    (space)
    (explain (s/and string? #(xss/string-of* % {:blank? false :min-length 3 :max-length 10})) ""))

  ;;   (space)
  ;;   (explain (s/and string? something?) ""))

  (do
    (space)
    (explain zero? 1))

  (do
    (space)
    (s/def ::c1 (s/map-of int? int? :count 3))
    (explain ::c1 {"a" "b"}))

  (do
    (space)
    (s/def ::c1 neg-int?)
    (explain ::c1 [1 1]))

  (do
    (space)
    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    (explain :foo/agent {:age 10}))

  (do
    (space)
    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    (explain :foo/agent {:age 10 :person {:names [1]}}))

  (do
    (space)
    (explain :foo/agent2 {:age ""}))

  (do
    (space)
    (s/def :foo/animal #{:a :b :c})
    (explain :foo/animal 1))

  (do
    (space)
    (explain-str :foo/person {:names [1 :yolo]}))

  (do
    (explain nil? 1)))

;; (explain (s/coll-of any? :max-count 3 :min-count 2) [1 2 3 4])
;; (require '[exoscale.specs.string :as xss])

;; (do
;;   (space)
;;   (explain (xss/string-of {:blank? true}) ""))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal (s/coll-of ::names))
;;   (println (explain-str ::animal [[::foo :bar]]))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal #{:a :b :c})
;;   (println (explain-str ::animal 1))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal #{:a :b :c})
;;   (println (explain-str ::animal "d"))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal2 ::animal)
;;   (println (explain-str ::animal2 1))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal2 ::animal)
;;   (println (explain-str ::animal2 "a"))
;;   (println))

;; (do
;;   (println (apply str (repeat 80 "-")))
;;   (s/def ::animal2 ::animal)
;;   (println (explain-str int? "1"))
;;   (println))
