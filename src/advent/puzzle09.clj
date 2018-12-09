(ns advent.puzzle09)

(defn turn [xs x]
  (into [x] (->> xs
                 cycle
                 (drop 2)
                 (take (count xs)))))
