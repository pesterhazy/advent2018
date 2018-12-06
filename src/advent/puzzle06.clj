(ns advent.puzzle06)

(defn read-sample
  []
  (with-open [f (-> "6/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))


(defn parse [xs]
  (->> xs
       (map #(clojure.string/split % #",\s*"))
       (mapv (partial mapv #(Long/parseLong %)))))

(def width 10)
(def height 10)

(defn edge? [[x y]]
  (or (= x 0)
      (= y 0)
      (= x (dec width))
      (= y (dec height))))
