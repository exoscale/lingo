(ns exoscale.lingo.utils
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as zn]
            [clojure.pprint :as pp]))

(defn- subpath?
  "True'ish if `x` is a subpath of `y`. Could use subvec but will do for now"
  [x y]
  (= (take (count x) y) x))

(defn focus
  "Takes a value, runs `mismatch-fn` on all values that are not in `paths` and
  runs `match-fn` on all values that are in `paths`. This is usefull to create
  values with every irrelevant values blanked and highlights on the relevant
  ones for instance"
  ([m path]
   (focus m
          path
          {}))
  ([m path opts]
   (focus m
          path
          opts
          []))
  ([m path {:keys [mismatch-fn match-fn descend-mismatching-nodes?]
            :or {mismatch-fn (constantly '_)
                 match-fn identity}
            :as opts}
    current-path]
   (cond
     (= path current-path)
     (match-fn m)

     (and (not descend-mismatching-nodes?)
          (not (subpath? current-path path)))
     (mismatch-fn m)

     (map? m)
     (into (empty m)
           (map (fn [[k v]]
                  [k
                   (focus v
                          path
                          opts
                          (conj current-path k))]))
           m)
     (coll? m)
     (into (empty m)
           (map-indexed (fn [idx x]
                          (focus x
                                 path
                                 opts
                                 (conj current-path idx))))
           m)
     :else (mismatch-fn m))))

(defn- zget-in
  [zloc ks]
  (loop [zloc zloc
         [k & ks] ks]
    (if k
      (recur (z/get zloc k) ks)
      zloc)))

(defn highlight
  [m path]
  (let [m-str (with-out-str (pp/pprint (focus m path)))
        zloc (z/of-string m-str {:track-position? true})
        num-lines (-> zloc z/rightmost* z/position first)]
    (when-let [zloc (zget-in zloc path)]
      (let [val-idx (-> zloc z/position second)
            val-len (count (str (z/sexpr zloc)))
            marker (->> (concat (repeat (- val-idx 1)
                                        " ")
                                (repeat val-len "^" ))
                        (apply str)
                        symbol)]
        (if (= num-lines 1)
          ;; if there is just one line, we can just concat the marker
          (str m-str marker)
          ;; more than 1 line, we must insert the marker between lines
          ;; and realign
          (some-> (z/skip z/next z/linebreak? zloc) ; go to end of line
                  z/next* ; skip lf
                  (z/insert-right* (zn/coerce marker)) ; insert marker
                  z/next ; skip marker
                  z/insert-newline-left
                  z/root-string))))))
