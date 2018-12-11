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
               (map (fn [pair] (apply ->level grid-sn pair)))
               (apply +))])
       blocks))

(defn solution-1 [blocks]
  (->> blocks
       evaluate
       (apply max-key second)
       first))
