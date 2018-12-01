(ns advent.puzzle01)

(def sample-changes
  [1 -2 3 1])

(defn read-changes
  []
  (with-open [f (-> "1/input.txt"
                    clojure.java.io/reader)]
    (->> (line-seq f)
         (mapv #(Long/parseLong %)))))

(defn calc [changes]
  (let [step (fn [[acc-freq acc-seen] n]
               (let [next-freq (+ acc-freq n)]
                 (if (acc-seen next-freq)
                   (reduced next-freq)
                   [next-freq (conj acc-seen next-freq)])))
        result (->> changes
                    cycle
                    (reduce step [0 #{}]))]
    (if (number? result)
      result
      nil)))

(defn solution
  []
  (calc (read-changes)))

(defn -main []
  (println (solution)))
