# lingo

> The language and speech, especially the jargon, slang, or argot, of
> a particular field, group, or individual

Trying to make spec explain message more human readable with small
additions.

## Documentation

Adds 3 functions similar to clojure.spec.alpha/explain-*

* `exoscale.lingo/explain-data`: returns
  clojure.spec.alpha/explain-data for spec/value with extra fields

* `exoscale.lingo/explain`: uses exoscale.lingo/explain-data but prints
  a nicely formated message)

* `exoscale.lingo/explain-str`: same as exoscale.lingo/explain but
  returns a string instead of printing

`exoscale.lingo/explain-data` is the most important one really

It will take a spec, a value and potentially options and return clojure.spec
explain-data for it with extra fields added.

For now it extends clojure.spec.alpha/problems with
`exoscale.lingo/message` and `exoscale.lingo/path`:

```clj
(s/def :foo/person (keys :req-un [:foo/names]))
(s/def :foo/names (s/coll-of :foo/name))
(s/def :foo/name string?)

(exoscale.lingo/explain-data :foo/person {:names [1 :yolo]})

#:clojure.spec.alpha{:problems
                     ({:path [:names],
                       :pred clojure.core/string?,
                       :val 1,
                       :via [:foo/person :foo/names :foo/name],
                       :in [:names 0],
                       :exoscale.lingo/message "should be a String",
                       :exoscale.lingo/path "names[0]"}
                      {:path [:names],
                       :pred clojure.core/string?,
                       :val :yolo,
                       :via [:foo/person :foo/names :foo/name],
                       :in [:names 1],
                       :exoscale.lingo/message "should be a String",
                       :exoscale.lingo/path "names[1]"}),
                     :spec :foo/person,
                     :value {:names [1 :yolo]}}
```

There are 2 ways to specify custom messages, depending on what is the
source you start from:

If you are working from spec identifiers or static forms you can use
`set-spec-error!`, it will dispatch on the problem spec, potentially
resolving aliases too, up to the pred failing at the end:

``` clj
(set-spec-error! `string? "should be a String")
(set-spec-error! ::thing "should be a Thing")
(set-spec-error! (s/coll-of ::thing) "should be a collection of Things")
```

What I meant by "resolving aliases" is that for something like this

``` clj
(s/def ::foo ::bar)
(s/def ::bar ::baz)
```

If you have a custom message on ::baz or ::bar and your value blows up
at ::foo level, lingo will pick up the first message in the alias
chain (so checks ::foo, then ::bar and then ::baz). Alias information
is not data available from raw explain-data, lingo has to infer by itself.

If you want to have more precise error handling based on the problem
pred only (usually it's the best things to do) you can use `set-pred-error!`.

``` clj
(set-pred-error! (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
                        :min number?
                        :max number?
                        :_ #{'%})
                 (fn [{:keys [min max]} _opts]
                   (format "should be an Integer between %d %d" min max)))
```

This will use the first argument to perform conforming against the
pred in the explain-data problems and use the bound values with second
argument, a function to generate a precise message.

so for an error like this:

```clj
#:clojure.spec.alpha{:problems
                     [{:path [],
                       :pred
                       (clojure.core/fn
                        [%]
                        (clojure.spec.alpha/int-in-range? 0 3 %)),
                       :val 4,
                       :via [],
                       :in []}],
                     :spec
                     #object[clojure.spec.alpha$and_spec_impl$reify__blabla]
                     :value 4}
```



It will pass the abbreviated `(clojure.spec.alpha/int-in-range? 0 3
%)` to the conforming spec we defined.

``` clj
(s/cat :_ #{'clojure.spec.alpha/int-in-range?}
                        :min number?
                        :max number?
                        :_ #{'%})
```

Which will destructure it to `{:min 0 :max 3}` and call the following
function on it.

``` clj
(fn [{:keys [min max]} _opts]
   (format "should be an Integer between %d %d" min max))
```

This is a trivial example, but if you take a s/coll-of (or any of
s/every variants), which can return a miriad of `preds` depending on
how failure happened, this will return very fine grained error message
that pin-point exactly how the value failed.
This is also very handy for more "custom" other uses cases, such as
`exoscale.specs/string-of` and other paramerized specs we might have.

By default a lot of common predicates are supported out of the box ,
most math comparaison operators and compositions of these with `count`
for instance, Set/Map membership (or lack thereof), and most of
clojure.spec custom predicates.

The tests demonstrate some of these :
https://github.com/exoscale/lingo/blob/master/test/exoscale/lingo/test/core_test.cljc

## License

Copyright © 2021 [Exoscale](https://exoscale.com)
