# lingo

> The language and speech, especially the jargon, slang, or argot, of
> a particular field, group, or individual

Trying to make spec explain message more usable with small, composable additions.

The goal is to provide the spec users with more data from errors and means to
render helpful error messages.

It differs from other similar libraries in that the focus is more on
explain-data enrichment first, then leveraging this for potential rendering.

In some cases it's better to leave the `problems` as a specialized collection
items and be very detailed per problem, in others you prefer to group things to
show more compact errors (typically like showing a set of missing keys in a
map).

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

Some very high level:

* `:exoscale.lingo.explain/message` infered error message you might want to display
* `:exoscale.lingo.explain/path` humanized path
* `:exoscale.lingo.explain/highlight` potential highlight output (shows error
  value in context with blanked suroundings)

Keys related to a potential custom error message found at spec level:

* `:exoscale.lingo.explain.spec/spec` which spec had a message registered for it
* `:exoscale.lingo.explain.spec/message` the message in question

Keys related to a potential error infered from pred data
* `:exoscale.lingo.explain.pred/spec` the spec used to match the predicate (via conform)
* `:exoscale.lingo.explain.pred/vals` the values destructured via conformed
* `:exoscale.lingo.explain.pred/message` the message generated from the values extracted + potential formater registered for this predicate key

So that's quite a lot of information, in most cases you will just care about the
first three, but the rest is available to you via explain-data if you need/want
to build your own outputs. If you look at the way `lingo/explain-data` is
implemented you will also see that all this is very composable you can easily
enrich/lighten this information.

Ok, now examples:

```clj
(s/def :foo/person (keys :req-un [:foo/names]))
(s/def :foo/names (s/coll-of :foo/name))
(s/def :foo/name string?)

(exoscale.lingo/explain :foo/person {:names [1 :yolo]})

1 in `names[0]` is an invalid :foo/name - should be a String

{:names [1 _]}
         ^

(exoscale.lingo/explain-data :foo/person {:names [1 :yolo]})

#:clojure.spec.alpha{:problems
                     ({:path [:names],
                       :exoscale.lingo.explain.pred/spec
                       :exoscale.lingo.pred/symbol,
                       :pred clojure.core/string?,
                       :exoscale.lingo.explain/highlight
                       "{:names [1 _]}\n         ^",
                       :via [:foo/person :foo/names :foo/name],
                       :exoscale.lingo.explain.spec/spec clojure.core/string?,
                       :val 1,
                       :exoscale.lingo.explain.pred/message
                       "should be a String",
                       :exoscale.lingo.explain/message "should be a String",
                       :exoscale.lingo.explain.spec/message
                       "should be a String",
                       :exoscale.lingo.explain.pred/vals clojure.core/string?,
                       :exoscale.lingo.explain/path "names[0]",
                       :in [:names 0]}
                      {:path [:names],
                       :exoscale.lingo.explain.pred/spec
                       :exoscale.lingo.pred/symbol,
                       :pred clojure.core/string?,
                       :exoscale.lingo.explain/highlight
                       "{:names [_ :yolo]}\n           ^^^^^",
                       :via [:foo/person :foo/names :foo/name],
                       :exoscale.lingo.explain.spec/spec clojure.core/string?,
                       :val :yolo,
                       :exoscale.lingo.explain.pred/message
                       "should be a String",
                       :exoscale.lingo.explain/message "should be a String",
                       :exoscale.lingo.explain.spec/message
                       "should be a String",
                       :exoscale.lingo.explain.pred/vals clojure.core/string?,
                       :exoscale.lingo.explain/path "names[1]",
                       :in [:names 1]}),
                     :spec :foo/person,
                     :value {:names [1 :yolo]}}
```

As you can see there's a lot of more information available than what spec returns alone.

There are 2 ways to specify custom messages, depending on what is the source you
start from:

If you are working from spec identifiers or static forms you can use
`set-spec-error!`, it will dispatch on the problem spec (value we get from
explain-data `:via`), potentially resolving aliases too, up to the pred failing
at the end. spec errors will result in addition of
`:exoscale.lingo.explain.spec/*` keys to the problem data.

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


If you want to have more precise error handling based on the problem pred only
(usually it's the best things to do) you can use `set-pred-error!`. This will
result in the addition of the `:exoscale.lingo.explain.pred/*` keys to the
problem.

``` clj
(set-pred-error! (s/def ::int-in-range (s/cat :_ #{'clojure.spec.alpha/int-in-range?}
                        :min number?
                        :max number?
                        :_ #{'%}))
                 (fn [{:keys [min max]} _opts]
                   (format "should be an Integer between %d %d" min max)))
```

This will use the first argument to perform conforming against the
pred in the explain-data problems and use the bound values with second
argument, a function to generate a precise message.

Internally these are really 2 distinct operations, we first destructure via the
spec then render in another step.

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


`set-pred-error!` calls internally `set-pred-conformer!` and
`set-pred-message!`, the operations are decoupled. If you only care about the
destructuring just set a pred conformer for your use and leave the formater out.

The tests demonstrate some of these :
https://github.com/exoscale/lingo/blob/master/test/exoscale/lingo/test/core_test.cljc

###Options

* `:registry` defaults to lingo's internal registry, but you can have your own, could be handy if you need to support multiple languages for instance
* `:conform` defaults to `(memoize s/conform)` the function used to destructure predicate forms
* `:highlight?` defaults to true, whether we should try to provide an highlight of the error value
* `:highlight-inline-message?` defaults to false, whether we should show the explain message inline with the error marker in the highlight
* `:highlight-colors?` defaults to false, whether to use terminal colors with the highlight

## License

Copyright © 2022 [Exoscale](https://exoscale.com)
