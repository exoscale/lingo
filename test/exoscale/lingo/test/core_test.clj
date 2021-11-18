(ns exoscale.lingo.test.core-test
  (:require [clojure.test :refer [are deftest]]
            [exoscale.lingo :as l]
            [exoscale.specs :as xs]
            [exoscale.specs.string :as xss]
            [clojure.spec.alpha :as s]))

(defn f2? [_] false)
(defn f3? [_] false)

(l/with-error! `exoscale.lingo.test.core-test/f2? "yolo")
(l/with-error! `f3? "should match Something")

(-> (s/def ::thing #(string? %))
    (l/with-error! "should be a string with bla bla bla"))

(s/def ::things (s/coll-of ::thing))

(s/def :foo/name string?)

(s/def :foo/names (s/coll-of :foo/name))

(s/def :foo/person (s/keys :req-un [:foo/names]))

(s/def :foo/age int?)
(s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))

(s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))

(do
  (println "-------------------------")
  (l/explain-data ::things 1))

(deftest test-outputs
  (are [spec val output] (= (l/explain-str spec val) output)

    ::thing
    1
    "1 is an invalid :exoscale.lingo.test.core-test/thing - should be a string with bla bla bla\n"

    (s/coll-of ::thing)
    [1]
    "1 in `[0]` is an invalid :exoscale.lingo.test.core-test/thing - should be a string with bla bla bla\n"

    ::things
    [1]
    "1 in `[0]` is an invalid :exoscale.lingo.test.core-test/thing - should be a string with bla bla bla\n"

    ;; test traversing
    (s/def ::things2 ::things)
    [1]
    "1 in `[0]` is an invalid :exoscale.lingo.test.core-test/thing - should be a string with bla bla bla\n"

    ::things
    1
    "1 is an invalid :exoscale.lingo.test.core-test/things - should be a Collection\n"

    (s/and string? #(> (count %) 3))
    ""
    "\"\" is invalid - should contain more than 3 elements\n"

    ;; test the original unchanged msg
    (s/and string? #(pos? (count %)))
    ""
    "\"\" is invalid - (pos? (count %))\n"

    ;; with a custom pred matcher
    (do
      (l/register-matcher! '(pos? (count %)) "should be non blank")
      (s/and string? #(pos? (count %))))
    ""
    "\"\" is invalid - should be non blank\n"

    #{:a :b :c}
    "b"
    "\"b\" is invalid - should be one of :a,:b,:c\n"

    (s/and string? #(xss/string-of* % {:blank? false :min-length 3 :max-length 10}))
    ""
    "\"\" is invalid - should be a String non blank, at least 3 characters in length, at most 10 characters in length\n"
    (s/def :exoscale.lingo/c1 (s/map-of int? int? :count 3))
    {"a" "b"}
    "{\"a\" \"b\"} is an invalid :exoscale.lingo/c1 - should contain exactly 3 elements\n"

    (s/and any? #(= 1 (count %)))
    []
    "[] is invalid - should contain exactly 1 element\n"

    (s/and any? #(= (count %) 1))
    []
    "[] is invalid - should contain exactly 1 element\n"

    (s/and any? #(= 42 (count %)))
    []
    "[] is invalid - should contain exactly 42 elements\n"

    (s/and any? #(= (count %) 42))
    []
    "[] is invalid - should contain exactly 42 elements\n"

    (s/and any? #(>= (count %) 42))
    []
    "[] is invalid - should contain at least 42 elements\n"

    (s/and any? #(<= (count %) 1))
    [1 1]
    "[1 1] is invalid - should contain at most 1 element\n"

    (s/and any? #(<= % 1))
    10
    "10 is invalid - should be at most 1\n"

    (s/and any? #(< % 1))
    10
    "10 is invalid - should be less than 1\n"

    (s/and any? #(>= % 1))
    0
    "0 is invalid - should be at least 1\n"

    (s/and any? #(> % 1))
    0
    "0 is invalid - should be greater than 1\n"

    (s/and any? #(= % "yolo"))
    0
    "0 is invalid - should be equal to yolo\n"

    (s/and any? #(= "yolo" %))
    0
    "0 is invalid - should be equal to yolo\n"

    (s/int-in 0 10)
    -1
    "-1 is invalid - should be an Integer between 0 10\n"

    (s/double-in :min 0 :max 10)
    (double 11)
    "11.0 is invalid - should be at most 10\n"

    (s/coll-of any? :min-count 3)
    [1]
    "[1] is invalid - should contain at least 3 elements\n"

    (s/coll-of any? :max-count 3)
    [1 1 1 1]
    "[1 1 1 1] is invalid - should contain between 0 3 elements\n"

    (s/coll-of any? :max-count 3 :min-count 1)
    [1 1 1 1]
    "[1 1 1 1] is invalid - should contain between 1 3 elements\n"

    (s/coll-of any? :count 3)
    [1 1 1 1]
    "[1 1 1 1] is invalid - should contain exactly 3 elements\n"

    (s/coll-of any? :count 1)
    [1 1 1 1]
    "[1 1 1 1] is invalid - should contain exactly 1 element\n"

    (s/coll-of any? :kind set?)
    [1]
    "[1] is invalid - should be a Set\n"

    (s/map-of any? any? :count 1)
    {:a 1 :b 2}
    "{:a 1, :b 2} is invalid - should contain exactly 1 element\n"

    neg-int?
    [1]
    "[1] is invalid - should be a Negative Integer\n"

    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    {:age 10}
    "{:age 10} is an invalid :foo/agent - missing key :person\n"

    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    {:age 10 :person {:names [1]}}
    "1 in `[:person :names 0]` is an invalid :foo/name - should be a String\n"

    (-> (s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))
        ;; (xs/with-meta! {:exoscale.lingo/name "Agent"})
        )
    {:age ""}
    "\"\" in `[:age]` is an invalid :foo/age - should be an Integer\n{:age \"\"} is an invalid :foo/agent2 - missing key :person\n"

    (s/def :foo/animal #{:a :b :c})
    1
    "1 is an invalid :foo/animal - should be one of :a,:b,:c\n"

    :foo/person
    {:names [1 :yolo]}
    "1 in `[:names 0]` is an invalid :foo/name - should be a String\n:yolo in `[:names 1]` is an invalid :foo/name - should be a String\n"

    nil?
    1
    "1 is invalid - should be nil\n"

    f2?
    1
    "1 is invalid - yolo\n"

    f3?
    1
    "1 is invalid - should match Something\n"))
