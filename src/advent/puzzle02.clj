(ns advent.puzzle02)

(def sample-codes
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(def sample-codes2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn read-codes
  []
  (with-open [f (-> "2/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn checksum
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
  [xs]
  (->> xs
       pairs
       (some (fn [pair] (when (= 1 (apply distance pair)) pair)))))
