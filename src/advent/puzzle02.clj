(ns advent.puzzle02)

(def sample-codes
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

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
