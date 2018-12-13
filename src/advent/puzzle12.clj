(ns advent.puzzle12
  (:import java.util.BitSet)
  (:require [clojure.string :as str]))

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
  [s]
  (->> (range (.length s))
       (map (fn [idx] (if (.get s idx) \# \.)))
       (str/join)))

(def regex #"initial state: (.+)")

(defn parse-header
  [line]
  (let [s (->> (re-matches regex line)
               second)]
    {:length (count s)
     :initial-state (s->bitset (str (apply str (repeat padding \.)) s))}))

(def regex2 #"^([#.]+) => ([#.])$")

(defn parse-body-line
  [line]
  (let [[_ k v] (re-matches regex2 line)]
    (prn k v)
    [(-> k
         s->bitset
         .toLongArray
         first) (= "#" v)]))

(defn parse [[fst _ & body]]
  (-> (parse-header fst)
      (assoc :lookup (->> body (map parse-body-line) (into {})))))
