(ns exoscale.lingo.test.core-test
  (:require [clojure.test :refer [are deftest is]]
            [exoscale.lingo :as l]
            [exoscale.lingo.utils :as u]
            ;; [exoscale.specs.string :as xss]
            [clojure.spec.alpha :as s]))

(defn f2? [_] false)
(defn f3? [_] false)

(l/set-spec-error! `exoscale.lingo.test.core-test/f2? "yolo")
(l/set-spec-error! `f3? "should match Something")

(-> (s/def ::thing #(string? %))
    (l/set-spec-error! "should be a string with bla bla bla"))

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

(def ^:dynamic *opts* {})

(deftest test-outputs
  (are [spec val output] (= (l/explain-str spec val *opts*)
                            output)

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

    (s/def ::cnt #(> (count %) 3))
    ""
    "\"\" is an invalid :exoscale.lingo.test.core-test/cnt - should contain more than 3 elements\n"

    ;; test the original unchanged msg
    (s/and string? #(pos? (count %)))
    ""
    "\"\" is invalid - (pos? (count %))\n"

    ;; with a custom pred matcher
    (do
      (l/set-pred-error! #{'(pos? (count %))} (constantly "should be non blank"))
      (s/and string? #(pos? (count %))))
    ""
    "\"\" is invalid - should be non blank\n"

    #{:a :b :c}
    "b"
    "\"b\" is invalid - should be one of :a, :b, :c\n"

    ;; (s/and string? #(xss/string-of* % {:blank? false :min-length 3 :max-length 10}))
    ;; ""
    ;; "\"\" is invalid - should be a String non blank, at least 3 characters in length, at most 10 characters in length\n"
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

    (s/and number? #(<= 0 % 10))
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

    (s/def :foo/agent (s/keys :req [:foo/person :foo/age]))
    {:foo/age 10}
    "#:foo{:age 10} is an invalid :foo/agent - missing key :foo/person\n"


    (do
      (alter-var-root #'*opts* assoc :hide-keyword-namespaces? true)
      (s/def :foo/agent (s/keys :req [:foo/person :foo/age])))
    {:foo/age 10}
    "#:foo{:age 10} is an invalid :foo/agent - missing key :person\n"

    (do
      (alter-var-root #'*opts* dissoc :hide-keyword-namespaces?)
      (s/def :foo/agent (s/keys :req-un [:foo/person :foo/age])))
    {:age 10 :person {:names [1]}}
    "1 in `person.names[0]` is an invalid :foo/name - should be a String\n"

    (-> (s/def :foo/agent2 (s/keys :req-un [:foo/person :foo/age]))
        ;; (xs/with-meta! {:exoscale.lingo/name "Agent"})
        )
    {:age ""}
    "\"\" in `age` is an invalid :foo/age - should be an Integer\n{:age \"\"} is an invalid :foo/agent2 - missing key :person\n"

    (s/def :foo/animal #{:a :b :c})
    1
    "1 is an invalid :foo/animal - should be one of :a, :b, :c\n"

    :foo/person
    {:names [1 :yolo]}
    "1 in `names[0]` is an invalid :foo/name - should be a String\n:yolo in `names[1]` is an invalid :foo/name - should be a String\n"

    nil?
    1
    "1 is invalid - should be nil\n"

    (s/nilable string?)
    1
    "1 is invalid - should be a String\n1 is invalid - should be nil\n"

    f2?
    1
    "1 is invalid - yolo\n"

    f3?
    1
    "1 is invalid - should match Something\n"))

(deftest focus-test
  (let [_ '_]
    (is (= [_ _ _] (u/focus [3 2 1] nil)))
    (is (= _ (u/focus 1 nil)))
    (is (= 1 (u/focus 1 [])))

    (is (= [_ _ 1] (u/focus [3 2 1] [2])))
    (is (= [3 _ _] (u/focus [3 2 1] [0])))

    (is (= {:a 1} (u/focus {:a 1} [:a])))
    (is (= {:a _} (u/focus {:a 1} [:b])))
    (is (= {:a _ :c 1} (u/focus {:a {:b 1} :c 1} [:c])))

    (is (= {:a {:b [_ {:c {:d #{:b :a}, :e _}}]}}
           (u/focus {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
                    [:a :b 1 :c :d]
                    {:descend-mismatching-nodes? true})))

    (is (= {:a {:b [1 {:c {:d #{_}, :e _}}]}}
           (u/focus {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
                    [:a :b 0]
                    {:descend-mismatching-nodes? true})))

    (is (= {:a {:b [_ {:c {:d #{_}, :e _}}]}}
           (u/focus {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
                    nil
                    {:descend-mismatching-nodes? true})))

    (is (= {:a {:b [1 _]}}
           (u/focus {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
                    [:a :b 0])))

    (is (= {:a {:b [_ {:c {:d #{:b :a} :e _}}]}}
           (u/focus {:a {:b [1 {:c {:d #{:b :a} :e {:f 1}}}]}}
                    [:a :b 1 :c :d])))))

(deftest highlight-test
  (are [input path output]
      (= (u/highlight input path)
         output)

    [3 2 1] [2] "[_ _ 1]\n     ^"

    [3 2 1] [0] "[3 _ _]\n ^"

    {:a 1} [:a] "{:a 1}\n    ^"

    {:a {:b 1} :c 1} [:c] "{:a _, :c 1}\n          ^"

    {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
    [:a :b 1 :c :d]
    "{:a {:b [_ {:c {:d #{:b :a}, :e _}}]}}\n                   ^^^^^^^^"

    {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
    [:a :b 0]
    "{:a {:b [1 _]}}\n         ^"

    {:a {:b [1 {:c {:d #{:a :b} :e :foo}}]}}
    [:a :b 0]
    "{:a {:b [1 _]}}\n         ^"

    {:a {:b [1 {:c {:d #{:b :a} :e {:f 1}}}]}}
    [:a :b 1 :c :d]
    "{:a {:b [_ {:c {:d #{:b :a}, :e _}}]}}\n                   ^^^^^^^^"

    ;; single line hl
    {:a {:bar 255555 :c 3 :d 4 :e 5}}
    [:a :bar]
    "{:a {:bar 255555, :c _, :d _, :e _}}\n          ^^^^^^"

    ;; multiline hl output
    {:aaaaaaaaaaaaa
     {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2 :c 33333 :d 4 :e 5}}
    [:aaaaaaaaaaaaa
     :c]
    "{:aaaaaaaaaaaaa\n {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd _,\n  :c 33333,\n     ^^^^^\n  :d _,\n  :e _}}"))
