(ns exoscale.lingo.utils)

(defn string-builder
  ([] (StringBuilder.))
  ([^StringBuilder sb x] (.append sb x))
  ([^StringBuilder sb] (.toString sb)))
