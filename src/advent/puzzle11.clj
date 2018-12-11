(ns advent.puzzle11)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def grid-sn 4151)

;; Find the fuel cell's rack ID, which is its X coordinate plus 10.
;; Begin with a power level of the rack ID times the Y coordinate.
;; Increase the power level by the value of the grid serial number (your puzzle input).
;; Set the power level to itself multiplied by the rack ID.
;; Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
;; Subtract 5 from the power level.

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

;; The rack ID is 3 + 10 = 13.
;; The power level starts at 13 * 5 = 65.
;; Adding the serial number produces 65 + 8 = 73.
;; Multiplying by the rack ID produces 73 * 13 = 949.
;; The hundreds digit of 949 is 9.
;; Subtracting 5 produces 9 - 5 = 4.

(defn solution-1 [blocks]
  (->> blocks
       (map (fn [[k pairs]]
              [k
               (->> pairs
                    (map (fn [pair] (apply ->level grid-sn pair)))
                    (apply +))]))
       (apply max-key second)
       first))
