(ns exoscale.lingo.highlight
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

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
                   (focus v path opts (conj current-path k))]))
           m)
     (coll? m)
     (into (empty m)
           (map-indexed (fn [idx x]
                          (focus x path opts (conj current-path idx))))
           m)
     :else (mismatch-fn m))))

(defn- string-builder
  ([] (StringBuilder.))
  ([^StringBuilder sb x] (.append sb x))
  ([^StringBuilder sb] (.toString sb)))

(defn- marker
  [offset len]
  (->> (concat (repeat offset " ")
               (repeat len "^"))
       (apply str)))

(defn- pp-str
  [x]
  (let [s (with-out-str (pp/pprint x))]
    (subs s 0 (dec (count s)))))

(defn- width
  [s]
  (reduce (fn [x l]
            (let [len (count l)]
              (if (> len x)
                len
                x)))
          0
          (str/split-lines s)))

(defn- pad [i]
  (reduce string-builder
          (string-builder)
          (repeat i \space)))

(defn- justify-error [s idx]
  (let [pad' (pad idx)]
    (transduce (comp (map-indexed (fn [i s]
                                    (cond->> s
                                      (not (zero? i))
                                      (str pad'))))
                     (interpose \newline))
               string-builder
               (str/split-lines s))))

(defn prefix-lines [s prefix]
  (transduce (comp (map (fn [s] (str prefix s)))
                   (interpose \newline))
             string-builder
             (str/split-lines s)))

(def ^:private relevant-mark 'exoscale.lingo/relevant)

(defn- relevant-mark-index
  [line]
  (str/index-of line (str relevant-mark)))

(defn- replace-mark
  [line val idx]
  (str/replace line
               (str relevant-mark)
               (justify-error val idx)))

(defn- prep-val
  "Replaces error value with placeholder, then pprint without newline char at the end"
  [m in]
  (pp-str (focus m in {:match-fn (constantly relevant-mark)})))

(def colors
  {:red "\u001b[31m"
   :yellow "\u001b[33m"
   :blue "\u001b[34m"
   :cyan "\u001b[36;1m"
   :reset "\u001b[0m"})

(defn color [s color]
  (str (color colors) s (:reset colors)))

(defn highlight
  [value
   {:as _pb
    :keys [in val pred]
    :exoscale.lingo.explain/keys [message]
    :or {message (str "Does not conform to " pred)}}
   {:as _opts :keys [colors?
                     highlight-inline-message?]}]
  (cond-> (->> (prep-val value in)
               str/split-lines
               (transduce (comp
                           (map (fn [line]
                                   ;; if line contains relevant value, add placholder
                                   ;; with rendered error
                                  (if-let [idx (relevant-mark-index line)]
                                    (let [s (pp-str val)]
                                      (str (replace-mark line
                                                         (cond-> s
                                                           colors?
                                                           (color :red))
                                                         idx)
                                           \newline
                                           (cond-> (str (marker idx (width s)))

                                             highlight-inline-message?
                                             (str " " message)

                                             colors?
                                             (color :red))
                                            ;; (str \newline (pad idx))
                                           ))
                                    line)))
                           (interpose \newline))
                          string-builder))))
