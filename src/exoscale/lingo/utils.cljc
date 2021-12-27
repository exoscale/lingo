(ns exoscale.lingo.utils
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

(defn- justify [s idx]
  (let [pad' (pad idx)]
    (transduce (comp (map-indexed (fn [i s]
                                    (cond->> s
                                      (not (zero? i))
                                      (str pad'))))
                     (interpose \newline))
               string-builder
               (str/split-lines s))))

(def ^:private relevant-mark 'exoscale.lingo/relevant)

(defn- relevant-mark-index
  [line]
  (str/index-of line (str relevant-mark)))

(defn- replace-mark
  [line val idx]
  (str/replace line
               (str relevant-mark)
               (justify val idx)))

(defn- prep-val
  "Replaces error value with placeholder, then pprint without newline char at the end"
  [m in]
  (pp-str (focus m in {:match-fn (constantly relevant-mark)})))

(defn highlight
  [value
   {:as _pb :keys [in val] :exoscale.lingo/keys [message]}
   {:as _opts :exoscale.lingo/keys [highlight-inline-message?]}]
  (->> (prep-val value in)
       str/split-lines
       (transduce (comp
                   (map (fn [line]
                          (if-let [idx (relevant-mark-index line)]
                            (let [s (pp-str val)]
                              (str (replace-mark line s idx)
                                   \newline
                                   (marker idx (width s))
                                   (when (and highlight-inline-message? message)
                                     (str \newline (pad idx) message))))
                            line)))
                   (interpose \newline))
                  string-builder)))

;; (print (highlight {:aaaaaaaaaaaaa
;;                    {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2
;;                     :c 33333
;;                     :d '[a b c]
;;                     :e 5
;;                     :f 1}}
;;                   {:in [:aaaaaaaaaaaaa
;;                         :d]
;;                    :exoscale.lingo/message "Should be a String"
;;                    }))

;; (print (highlight {:aaaaaaaaaaaaa
;;                    {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2
;;                     :c 33333
;;                     :d 44
;;                     :e 5
;;                     :f 1}}
;;                   {:in [:aaaaaaaaaaaaa
;;                         :d]}))
;; (print)
;; (print (highlight {:aaaaaaaaaaaaaa
;;                    {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2
;;                     :c 33333
;;                     :d 4
;;                     :e 5
;;                     :f {:aaaaaaaaaaaaa
;;                         {:bbbbbbbbbbbbbbbbbdddddddddddddddddddddddddddddddddddddd 2 :c 33333 :d 4 :e 5}}}}
;;                   {:in [:aaaaaaaaaaaaaa
;;                         :f]
;;                    :exoscale.lingo/message "Should be a string"}))
