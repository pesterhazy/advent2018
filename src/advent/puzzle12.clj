(ns advent.puzzle12
  (:import java.util.BitSet)
  (:require [clojure.string :as str]))

(def padding 2)
(def blength 5)

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
        (str/join)))
  )

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

(defn sub-bitset [bs from to]
  (let [new-bs (BitSet.)]
    (doseq [[new-idx old-idx] (map-indexed vector (range from to))]
      (.set new-bs new-idx (if (neg? old-idx)
                             false
                             (.get bs old-idx))))
    new-bs))

(defn next-gen
  [ctx bs]
  (doseq [idx (range (- padding) (:length ctx))]
    (let [v (if (neg? idx) false (.get bs idx))]
      (println (format "%3d" idx) (bitset->s (sub-bitset bs
                                                         (- idx padding)
                                                         (+ idx padding 1))
                                             blength))))
  bs)

(defn generations [ctx] (iterate #(next-gen ctx %) (:initial-state ctx)))
