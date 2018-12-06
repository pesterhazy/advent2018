(ns advent.puzzle06)

(defn read-sample
  []
  (with-open [f (-> "6/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "6/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn parse [xs]
  (->> xs
       (map #(clojure.string/split % #",\s*"))
       (mapv (partial mapv #(Long/parseLong %)))))

(def width 358)
(def height 353)
(def threshold 10000)

(defn edge? [[x y]]
  (or (= x 0)
      (= y 0)
      (= x (dec width))
      (= y (dec height))))

(defn edge-locations []
  (for [y (range height)
        x (range width)
        :when (edge? [x y])]
    [x y]))

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

(defn solution-1 []
  (let [grid (->> (read-input)
                  parse
                  (map-indexed vector)
                  (reduce (fn [grid [id coord]]
                            (color grid id coord))
                          {}))
        candidates (->> grid vals (keep first) frequencies)
        disqualified (->> (edge-locations)
                          (map grid)
                          (keep first)
                          set)]
    (apply max (keep (fn [[id area]]
                       (when-not (disqualified id)
                         area))
                     candidates))))


(defn total-distance [[x y] coords]
  (->> coords
       (map (fn [[coord-x coord-y]]
              (+ (Math/abs (- coord-x x))
                 (Math/abs (- coord-y y)))))
       (apply +)))

(defn solution-2 []
  (let [coords (->> (read-input) parse)]
    (->> (for [y (range height)
               x (range width)]
           (total-distance [x y] coords))
         (filter (fn [total-distance] (< total-distance threshold)))
         count)))
