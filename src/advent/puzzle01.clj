(ns advent.puzzle01)

(defn lines
  []
  (with-open [f (-> "1/input.txt"
                    clojure.java.io/reader)]
    (doall (line-seq f))))

(defn solution
  []
  (->> (lines)
       (map #(Long/parseLong %))
       (reduce + 0)))

(defn -main []
  (println (solution)))
