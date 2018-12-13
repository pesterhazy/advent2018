(ns advent.puzzle12
  (:import java.util.BitSet)
  (:require [clojure.string :as str]))

(def neighbors 2)
(def blength 5)
(def padding 3)

(defn read-sample
  []
  (with-open [f (-> "12/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn s->bitset
  [s]
  (let [bs (BitSet. (count s))]
    (doseq [[idx c] (map-indexed vector s)] (when (= \# c) (.set bs idx)))
    bs))

(defn bitset->s
  ([s]
   (->> (range (.length s))
        (map (fn [idx] (if (.get s idx) \# \.)))
        (str/join)))
  ([s length]
   (->> (range length)
        (map (fn [idx] (if (.get s idx) \# \.)))
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
  [bs from to]
  ;; FIXME: use fast path if from >= 0
  (let [new-bs (BitSet.)]
    (doseq [[new-idx old-idx] (map-indexed vector (range from to))]
      (.set new-bs new-idx (if (neg? old-idx) false (.get bs old-idx))))
    new-bs))

(defn bs->long
  [bs]
  (-> bs
      .toLongArray
      first))

(defn next-gen
  [{:keys [lookup]} [offset bs]]
  (let [new-bs (BitSet.)
        vs (->> (range (- 0 neighbors offset) (+ (.length bs) 2))
                (map (fn [idx]
                       (get lookup
                            (bs->long (sub-bitset bs
                                                  (- idx neighbors)
                                                  (+ idx neighbors 1)))
                            false))))
        [a b] (split-with false? vs)]
    (doseq [[idx v] (map-indexed vector b)] (.set new-bs idx v))
    [(- (count a) neighbors) new-bs]))

(defn bit-sum
  [bs]
  (loop [sum 0
         idx 0]
    (let [nxt (.nextSetBit bs idx)]
      (if (neg? nxt) sum (recur (+ sum idx) (inc idx))))))

(defn print-gen
  [[num [offset bs]]]
  (prn offset)
  (println (format "%2d" num)
           (str (apply str (repeat (+ padding offset) ".")) (bitset->s bs))
           (+ (bit-sum bs) (* offset (.cardinality bs)))))

(defn generations [ctx] (iterate #(next-gen ctx %) [0 (:initial-state ctx)]))
