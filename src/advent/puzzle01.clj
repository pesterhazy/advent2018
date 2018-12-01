(ns advent.puzzle01)

(def sample-deltas
  [1 -2 3 1])

(defn read-deltas
  []
  (with-open [f (-> "1/input.txt"
                    clojure.java.io/reader)]
    (->> (line-seq f)
         (mapv #(Long/parseLong %)))))

(defn frequency [deltas]
  (let [step (fn [[acc-freq acc-seen] n]
               (let [next-freq (+ acc-freq n)]
                 (if (acc-seen next-freq)
                   (reduced next-freq)
                   [next-freq (conj acc-seen next-freq)])))
        result (->> deltas
                    cycle
                    (reduce step [0 #{}]))]
    (if (number? result)
      result
      nil)))

(defn solution
  []
  (frequency (read-deltas)))

(defn -main []
  (println (solution)))

(comment
  (frequency sample-deltas)
  (frequency (read-deltas)))
