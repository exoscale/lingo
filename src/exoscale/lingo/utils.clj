(ns exoscale.lingo.utils)

(defn- subpath?
  "True'ish if `x` is a subpath of `y`. Could use subvec but will do for now"
  [x y]
  (= (take (count x) y) x))

(defn focus
  "Takes a value, runs `mismatch-fn` on all values that are not in `paths` and
  runs `match-fn` on all values that are in `paths`. This is usefull to create
  values with every irrelevant values blanked and highlights on the relevant
  ones for instance"
  ([m paths]
   (focus m
          (set paths)
          {}))
  ([m paths opts]
   (focus m
          (set paths)
          opts
          []))
  ([m paths {:keys [mismatch-fn match-fn descend-mismatching-nodes?]
             :or {mismatch-fn (constantly '_)
                  match-fn identity}
             :as opts}
    current-path]
   (cond
     (contains? paths current-path)
     (match-fn m)

     (and (not descend-mismatching-nodes?)
          (not (some #(subpath? current-path %) paths)))
     (mismatch-fn m)

     (map? m)
     (into (empty m)
           (map (fn [[k v]]
                  [k
                   (focus v
                          paths
                          opts
                          (conj current-path k))]))
           m)
     (coll? m)
     (into (empty m)
           (map-indexed (fn [idx x]
                          (focus x
                                 paths
                                 opts
                                 (conj current-path idx))))
           m)
     :else (mismatch-fn m))))
