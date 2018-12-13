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

(defn read-input
  []
  (with-open [f (-> "12/input.txt"
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

(defn bit-seq
  ([bs] (bit-seq bs 0))
  ([bs idx]
   (lazy-seq
    (let [nxt (.nextSetBit bs idx)]
      (cond (neg? nxt)
            '()
            (= nxt idx)
            (cons nxt (bit-seq bs (inc idx)))
            :else
            (bit-seq bs (inc idx)))))))

(defn calc [offset bs]
  (+ (->> bs bit-seq (reduce +)) (* offset (.cardinality bs))))

(defn print-gen
  [[num [offset bs]]]
  (println (format "%2d" num)
           (str (apply str (repeat (+ padding offset) ".")) (bitset->s bs))))

(defn generations [ctx] (iterate #(next-gen ctx %) [0 (:initial-state ctx)]))

(defn solution-1 []
  (let [gens (->> (read-input) parse generations)]
    (apply calc (nth gens 20))))
