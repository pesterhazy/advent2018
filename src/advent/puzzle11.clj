(ns advent.puzzle11)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def grid-sn 4151)

(defn hun ^long [^long n]
  (int (/ ^long (mod n 1000) 100)))

(defn ->level ^long [^long x ^long y]
  (- (hun (* (+ x 10) (+ (* y (+ x 10)) ^long grid-sn))) 5))

(def max-square-size 100)
(def width 300)
(def height 300)

(defn gen-blocks [^long size]
  (for [y (range (- ^long height size))
        x (range (- ^long width size))]
    [[x y]
     (for [y* (range y (+ ^long y size))
           x* (range x (+ ^long x size))]
       [x* y*])]))

(defn evaluate [blocks]
  (map (fn [[k pairs]]
         [k
          (->> pairs
               (map (fn [pair] (apply ->level  pair)))
               (apply +))])
       blocks))

(defn solution-1 []
  (->> (gen-blocks 3)
       evaluate
       (apply max-key second)
       first))

(defn hblocks
  "Returns all horizontal blocks for a given size

  An hblock has a vertical extension of 1 block"
  [^long size]
  (mapcat (fn [^long y]
            (map (fn [^long x]
                   [[x y size] (->> (range x (+ x size)) (reduce (fn ^long [^long acc ^long x*] (+ acc (->level x* y))) 0))])
                 (range (- ^long width (dec size)))))
          (range height)))

(defn all-hblocks []
  (->> (range 1 (inc ^long max-square-size))
       (reduce (fn [acc size]
                 (println size)
                 (into acc (hblocks size)))
               {})))

(defn solution-2 []
  (let [hblock->v (time (all-hblocks))]
    (->> (for [size (range 1 (inc ^long max-square-size))
               x (range 0 (- ^long width ^long size))
               y (range 0 (- ^long height ^long size))]
           [(->> (for [y* (range y (+ ^long y ^long size))]
                   (hblock->v [x y* size]))
                 (apply +))
            [x y size]])
         (apply max-key first))))
