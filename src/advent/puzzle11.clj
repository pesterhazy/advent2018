(ns advent.puzzle11)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def grid-sn 4151)

(defn hun ^long [^long n]
  (int (/ ^long (mod n 1000) 100)))

(defn ->level ^long [^long grid-sn ^long x ^long y]
  (- (hun (* (+ x 10) (+ (* y (+ x 10)) grid-sn))) 5))

(def width 300)
(def height 300)

(defn gen-blocks []
  (->> (for [y (range (- ^long height 3))
             x (range (- ^long width 3))]
         [[x y]
          (for [y* (range y (+ ^long y 3))
                x* (range x (+ ^long x 3))]
            [x* y*])])
       (into {})))

(defn solution-1 [blocks]
  (->> blocks
       (map (fn [[k pairs]]
              [k
               (->> pairs
                    (map (fn [pair] (apply ->level grid-sn pair)))
                    (apply +))]))
       (apply max-key second)
       first))
