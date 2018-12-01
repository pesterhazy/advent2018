(ns advent.puzzle01)

(def sample-deltas
  [1 -2 3 1])

(defn read-deltas
  []
  (with-open [f (-> "1/input.txt"
                    clojure.java.io/reader)]
    (->> (line-seq f)
         (mapv #(Long/parseLong %)))))

(defn step [[acc-freq acc-seen] n]
  (let [next-freq (+ acc-freq n)]
    (if (acc-seen next-freq)
      (reduced next-freq)
      [next-freq (conj acc-seen next-freq)])))

(defn frequency
  "Return a solution once a frequency is seen twice. Does not terminate if no
  solution is found"
  [deltas]
  (->> deltas
       cycle
       (reduce step [0 #{}])))

(defn frequency-steps
  "Lazily return all steps leading up to a solution, mostly for debugging"
  [deltas]
  (->> deltas
       cycle
       (reductions step [0 #{}])))

(defn solution
  []
  (frequency (read-deltas)))

(defn -main []
  (println (solution)))

(comment
  (frequency sample-deltas)
  (frequency (read-deltas))
  (count (frequency-steps (read-deltas))))
