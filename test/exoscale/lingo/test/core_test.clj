(ns exoscale.lingo.test.core-test
  (:require [clojure.test :refer [are deftest]]
            [exoscale.lingo :as l]
            [exoscale.specs :as xs]
            [exoscale.specs.string :as xss]
            [clojure.spec.alpha :as s]))

(-> (s/def :foo/name string?)
    (xs/with-meta! {::name "Entity Name"}))

(s/def :foo/names (s/coll-of :foo/name))

(-> (s/def :foo/person (s/keys :req-un [:foo/names]))
    (xs/with-meta! {::name "Person"}))

(s/def :foo/age int?)
(s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))

(-> (s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))
    (xs/with-meta! {::name "Agent"}))

(deftest test-outputs
  (are [spec val output] (= (l/explain-str spec val) output)
    ;; test the original unchanged msg
    (s/and string? #(pos? (count %)))
    ""
    "\"\" is invalid: (pos? (count %))\n"

    ;; with a custom pred matcher
    (do
      (l/def-pred-matcher '(pos? (count %)) "should be non blank")
      (s/and string? #(pos? (count %))))
    ""
    "\"\" is invalid: should be non blank\n"

    #{:a :b :c}
    "b"
    "\"b\" is invalid: should be one of :a,:b,:c\n"

    (s/and string? #(xss/string-of* % {:blank? false :min-length 3 :max-length 10}))
    ""
    "\"\" is invalid: should be a String non blank, at least 3 characters in length, at most 10 characters in length\n"
    (s/def :exoscale.lingo/c1 (s/map-of int? int? :count 3))
    {"a" "b"}
    "{\"a\" \"b\"} is invalid: should contain exactly 3 elements - spec: :exoscale.lingo/c1\n"

    neg-int?
    [1]
    "[1] is invalid: should match Negative Integer\n"

    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    {:age 10}
    "{:age 10} is invalid: missing key :person - spec: :foo/agent\n"

    (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age]))
    {:age 10 :person {:names [1]}}
    "1 is invalid: should match String in: [:person :names 0] - spec: :foo/name\n"


    (-> (s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))
        (xs/with-meta! {::name "Agent"}))
    {:age ""}
    "\"\" is invalid: should match Integer in: [:age] - spec: :foo/age\n{:age \"\"} is invalid: missing key :person - spec: :foo/agent2\n"

    (s/def :foo/animal #{:a :b :c})
    1
    "1 is invalid: should be one of :a,:b,:c - spec: :foo/animal\n"

    :foo/person
    {:names [1 :yolo]}
    "1 is invalid: should match String in: [:names 0] - spec: :foo/name\n:yolo is invalid: should match String in: [:names 1] - spec: :foo/name\n"

    nil?
    1
    "1 is invalid: should match nil\n"

    (s/int-in 0 10)
    -1
    "-1 is invalid: should be an Integer between 0 10\n"

    (s/double-in :min 0 :max 10)
    (double 11)
    "11.0 is invalid: should be smaller or equal than 10\n"

    (s/double-in :infinite? false)
    ##Inf
    "##Inf is invalid: cannot be Infinite\n"))
