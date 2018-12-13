(ns advent.puzzle12
  (:import java.util.BitSet)
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def neighbors 2)
(def blength 5)
(def padding 3)

(defn read-sample
  []
  (with-open [f (-> "12/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "12/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn ^BitSet s->bitset
  [^String s]
  (let [bs (BitSet. (count s))]
    (doseq [[idx c] (map-indexed vector s)] (when (= \# c) (.set bs idx)))
    bs))

(defn bitset->s
  ([^BitSet bs]
   (->> (range (.length bs))
        (map (fn [^long idx] (if (.get bs idx) \# \.)))
        (str/join)))
  ([^BitSet bs ^long length]
   (->> (range length)
        (map (fn [^long idx] (if (.get bs idx) \# \.)))
        (str/join))))

(def regex #"initial state: (.+)")

(defn parse-header
  [line]
  (let [s (->> (re-matches regex line)
               second)]
    {:length (count s), :initial-state (s->bitset s)}))

(def regex2 #"^([#.]+) => ([#.])$")

(defn parse-body-line
  [line]
  (let [[_ k v] (re-matches regex2 line)]
    [(-> k
         s->bitset
         .toLongArray
         first) (= "#" v)]))

(defn parse
  [[fst _ & body]]
  (-> (parse-header fst)
      (assoc :lookup (->> body
                          (map parse-body-line)
                          (into {})))))

(defn sub-bitset
  [^BitSet bs ^long from ^long to]
  ;; FIXME: use fast path if from >= 0
  (let [new-bs (BitSet.)]
    (doseq [[^long new-idx ^long old-idx] (map-indexed vector (range from to))]
      (let [v (if (neg? old-idx) false ^boolean (.get bs old-idx))]
        (.set new-bs
              ^long new-idx
              ^boolean v)))
    new-bs))

(defn ^long bs->long
  [^BitSet bs]
  (or (some-> bs .toLongArray first) 0))

(defn next-gen
  [{:keys [lookup]} [^long offset ^BitSet bs]]
  (let [new-bs (BitSet.)
        vs (->> (range (- 0 ^long neighbors offset) (+ (.length bs) 2))
                (map (fn [^long idx]
                       (get lookup
                            (bs->long (sub-bitset bs
                                                  (- idx ^long neighbors)
                                                  (+ idx ^long neighbors 1)))
                            false))))
        [a b] (split-with false? vs)]
    (doseq [[idx v] (map-indexed vector b)] (.set new-bs ^long idx ^boolean v))
    [(- (count a) ^long neighbors) new-bs]))

(defn bit-seq
  ([^BitSet bs] (bit-seq bs 0))
  ([^BitSet bs ^long idx]
   (lazy-seq
    (let [nxt (.nextSetBit bs idx)]
      (cond (neg? nxt)
            '()
            (= nxt idx)
            (cons nxt (bit-seq bs (inc idx)))
            :else
            (bit-seq bs (inc idx)))))))

(defn calc [^long offset ^BitSet bs]
  (let [a (long (apply + (bit-seq bs)))
        b ^long (* offset (.cardinality bs))]
    (+ a b)))

(defn print-gen
  [[^long num [^long offset bs]]]
  (println (format "%3d" num)
           (str (apply str (repeat (+ ^long padding offset) ".")) (bitset->s bs))
           (calc offset bs)))

(defn generations [ctx] (iterate #(next-gen ctx %) [0 (:initial-state ctx)]))

(defn solution-1 []
  (let [gens (->> (read-input) parse generations)]
    (apply calc (nth gens 20))))

(defn solution-2 []
  #_(let [gens (->> (read-input) parse generations)
          v (apply calc (nth gens 20))]
      (prn v)
      )
  (let [gens (->> (read-input) parse generations)]
    (reduce (fn [[seen? ^long idx] [offset bs]]
              (let [v (calc offset bs)]
                (when (= 0 (mod idx 100))
                  (prn idx v))
                (if (seen? v)
                  (reduced [idx (seen? v)])
                  [(assoc seen? v idx) (inc idx)])))
            [{} 0]
            gens)))
