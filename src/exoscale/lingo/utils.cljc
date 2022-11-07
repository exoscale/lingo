(ns exoscale.lingo.utils
  #?(:cljs (:import (google.string StringBuffer))))

#?:(:clj
    (defn string-builder
      ([] (StringBuilder.))
      ([^StringBuilder sb x] (.append sb x))
      ([^StringBuilder sb] (.toString sb)))
    :cljs
    (defn string-builder
      ([] (StringBuffer.))
      ([sb x] (.append sb x))
      ([sb] (.toString sb))))

(def colors
  {:red "\u001b[31m"
   :yellow "\u001b[33m"
   :blue "\u001b[34m"
   :cyan "\u001b[36;1m"
   :reset "\u001b[0m"})

(defn color [s color']
  (str (get colors color') s (:reset colors)))
