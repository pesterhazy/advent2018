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

(defn color
  "Takes a grid, a id and a coordinate [x y] and returns
  a new grid with points representing distance to coordinate"
  [grid id [coord-x coord-y]]
  (->> (for [y (range height)
             x (range width)]
         (let [[old-id old-distance :as v] (get grid [x y])
               distance (+ (Math/abs (- coord-x x))
                           (Math/abs (- coord-y y)))]
           [[x y]
            (cond
              (or (nil? v) (< distance old-distance))
              [id distance]

              (= distance old-distance)
              [nil distance]

              :else
              [old-id old-distance])]))
       (into {})))

(defn solution []
  (->> (read-sample)
       parse
       (map-indexed vector)
       (reduce (fn [grid [id coord]]
                 (color grid id coord))
               {})))
