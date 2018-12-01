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
  (->> changes
       (reduce + 0)))

(defn solution
  []
  (calc (read-changes)))

(defn -main []
  (println (solution)))
