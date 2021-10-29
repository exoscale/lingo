(ns exoscale.lingo
  (:require [clojure.spec.alpha :as s]
            [exoscale.specs :as xs]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [meander.epsilon :as m]))

(defn spec-name
  [spec]
  (get (xs/meta spec) ::name))

(defn spec-str
  [spec]
  (if-let [sn (spec-name spec)]
    (format " spec: %s (%s)" (pr-str spec) sn)
    (str " spec: " (pr-str spec))))

(defn strip-core
  [sym]
  (cond-> sym
    (= (namespace sym) "clojure.core")
    (-> name symbol)))

(def ^:dynamic *pred-matchers*
  '[[(contains? % ?key) (format "`missing key %s`" ?key)]
    [(m/pred set? ?set) (format "`should be one of %s`" (str/join "," (sort ?set)))]
    [(m/pred ident? ?id) (format "`should be a valid %s`" (or (spec-name ?id)
                                                              (strip-core ?id)))]

    ;; `every` (coll-of, etc...) :min-count, :max-count, :count
    [(<= ?min-count (count %) ?max-count) (format "`should contain between %s %s elements`"
                                                  ?min-count ?max-count)]
    [(= 1 (count %)) (format "`should contain only 1 element`")]
    [(= ?count (count %)) (format "`should contain exactly %s elements`"
                                  ?count)]])

(defn make-pred-matcher [ptns]
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
  (cond
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
                         :else form)))
                   form)
    :else form))

(defn pred-str
  [pred pred-matcher]
  (pred-matcher (abbrev pred)))

(defmacro add-pred-matcher!
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

(defn explain-str
  "Like spec explain-str, but uses lingo printer"
  [spec x]
  (with-out-str
    (binding [s/*explain-out* explain-printer]
      (s/explain spec x))))

(defn explain
  "Like spec explain, but uses lingo printer"
  [spec x]
  (binding [s/*explain-out* explain-printer]
    (s/explain spec x)))

;; set defaults for common preds

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
(xs/with-meta! `false? {::name "true"})
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; playground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(-> (s/def :foo/name string?)
    (xs/with-meta! {::name "Entity Name"}))

(s/def :foo/names (s/coll-of :foo/name))

(-> (s/def :foo/person (s/keys :req-un [:foo/names]))
    (xs/with-meta! {::name "Person"}))

(s/def :foo/age int?)
(s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))

(-> (s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))
    (xs/with-meta! {::name "Agent"}))

(defn space
  []
  (println)
  (println)
  (println (apply str (repeat 80 "-")))
  (println))

;; (binding [s/*explain-out* explain-printer]
;;   (space)
;;   (s/explain #{:a :b :c} "b"))

;; *pred-matcher*

(do
  (space)
  ;; (add-pred-matcher! '(pos? (count %)) "should be non blank")
  (explain (s/and string? #(pos? (count %))) ""))

(do
  (space)
  (add-pred-matcher! '(pos? (count %)) "should be non blank")
  (explain (s/and string? #(pos? (count %))) ""))

;; (require '[exoscale.specs.string :as xss])
;; (do
;;   (space)
;;   (add-pred-matcher! '(exoscale.specs.string/string-of* % ?pt)
;;                      '(format "should be a string with following attributes %s" ?pt))
;;   (explain (s/and string? (xss/string-of {:blank? false :min-length 3 :max-length 10})) ""))

;; (do
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
  (explain :foo/person {:names [1 :yolo]}))

;; (do
;;   (s/explain (s/int-in 0 20) ""))

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
