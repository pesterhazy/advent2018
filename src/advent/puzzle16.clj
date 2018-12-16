(ns advent.puzzle16
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "16/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def t-data (read-sample))

(def regex #"(?x)
Before:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]\n
(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\n
After:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]")

(defn match [lines]
  (let [matches (re-matches regex (str/join "\n" lines))]
    (assert matches)
    (->> (rest matches)
         (partition 4)
         (map (partial mapv #(Long/parseLong %)))
         (zipmap [:before :op :after]))))
