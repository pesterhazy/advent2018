(ns advent.puzzle02)

(def sample-codes
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

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
