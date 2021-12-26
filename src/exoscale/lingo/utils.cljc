(ns exoscale.lingo.utils
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; TODO tons of optimisations

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

(defn- string-builder
  ([] (StringBuilder.))
  ([^StringBuilder sb x] (.append sb x))
  ([^StringBuilder sb] (.toString sb)))

(defn- marker
  [offset len]
  (->>(concat
       (repeat offset
               " ")
       (repeat len "^"))
      (apply str)))

(defn- pp-str
  [x]
  (let [s (with-out-str (pp/pprint x))]
    (subs s 0 (dec (count s)))))

(defn- val-width
  [s]
  (reduce (fn [x l]
            (let [len (count l)]
              (if (> len x)
                len
                x)))
          0
          (str/split-lines s)))

(defn- justify [s idx]
  (let [pad (apply str (repeat idx " "))]
    (transduce (comp (map-indexed (fn [i s]
                                    (cond->> s
                                      (not (zero? i))
                                      (str pad))))
                     (interpose "\n"))
               string-builder
               (str/split-lines s))))

(def relevant-mark :exoscale.lingo/relevant)

(defn highlight
  [m path]
  (let [v (volatile! nil)
        m-str (pp-str
               (focus m
                      path
                      {:match-fn (fn [x]
                                   (vreset! v x)
                                   relevant-mark)}))
        lines (str/split-lines m-str)
        relevant-mark-str (str relevant-mark)]
    (transduce (comp
                (map (fn [line]
                       (if-let [idx (str/index-of line relevant-mark-str)]
                         (let [str-val (pp-str @v)]
                           (str (str/replace line
                                             relevant-mark-str
                                             (justify str-val idx))
                                "\n"
                                (marker idx (val-width str-val))))
                         line)))
                (interpose "\n"))
               string-builder
               lines)))

;; (print (highlight {:aaaaaaaaaaaaa
;;                    {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2
;;                     :c 33333
;;                     :d 44
;;                     :e 5
;;                     :f 1}}
;;                   [:aaaaaaaaaaaaa
;;                    :d]))
;; (print)
;; (print (highlight {:aaaaaaaaaaaaaa
;;                    {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2
;;                     :c 33333
;;                     :d 4
;;                     :e 5
;;                     :f {:aaaaaaaaaaaaa
;;                         {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2 :c 33333 :d 4 :e 5}}}}
;;                   [:aaaaaaaaaaaaaa
;;                    :f]))
