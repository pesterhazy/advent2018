(ns advent.puzzle02
  (:require [clojure.string]))

(def sample-codes
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(def sample-codes2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn read-codes
  []
  (with-open [f (-> "2/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn solution-1
  [codes]
  (let [{twos 2, threes 3} (->> codes
                                (mapcat (fn [code]
                                          (->> code
                                               frequencies
                                               vals
                                               (remove #{1})
                                               distinct)))
                                frequencies)]
    (* (or twos 0) (or threes 3))))

(defn distance
  [a b]
  (->> (map not= a b)
       (filter identity)
       count))

(defn pairs*
  "Eager version of pairs"
  [xs]
  (loop [acc []
         [fst & nxt] xs]
    (let [acc (into acc (map (fn [x] [fst x]) nxt))]
      (if nxt (recur acc nxt) acc))))

(defn pairs
  "Returns a lazy sequence containg pairwise combinations of xs"
  [xs]
  (lazy-seq (when-let [[fst & nxt] xs]
              (lazy-cat (map (fn [x] [fst x]) nxt) (pairs nxt)))))

(defn neighbors
  "Returns pair only if elements are neighbors"
  [pair]
  (when (= 1 (apply distance pair)) pair))

(defn shared-letters
  "Given two strings of the same length, return string containg only common
  letters"
  [a b]
  (->> (map vector a b)
       (keep (fn [[x y]] (when (= x y) x)))
       (clojure.string/join)))

(defn solution-2
  [xs]
  (some->> xs
           pairs
           (some neighbors)
           (apply shared-letters)))

(comment
  (solution-1 (read-codes))
  (solution-2 (read-codes)))
