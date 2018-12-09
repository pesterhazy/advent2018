(ns advent.puzzle09)

(defn nth-ccw [xs n] (nth xs (- (count xs) n)))

(defn insert-1
  "Insert a marble before the 2nd element"
  [xs x]
  (into [x]
        (->> xs
             cycle
             (drop 2)
             (take (count xs)))))

(defn backshift
  [xs backshift-pos]
  (->> xs
       cycle
       (drop (- (count xs) (dec backshift-pos)))
       (take (dec (count xs)))
       vec))

(defn simulate
  "Takes a sequence of marbles, returns score"
  [{:keys [n-players backshift-pos bingo]} marbles]
  (let [turn
        (fn [[xs score] x]
          (if (zero? (mod x bingo))
            (let [player (inc (mod (dec x) n-players))]
              [(backshift xs backshift-pos)
               (update score
                       player
                       (fn [n] (+ (or n 0) x (nth-ccw xs backshift-pos))))])
            [(insert-1 xs x) score]))]
    (second (reduce turn [[1] nil] marbles))))

(defn winner
  [n-players n-marbles]
  (->> (range 1 (inc n-marbles))
       (simulate {:backshift-pos 7, :bingo 23, :n-players n-players})
       vals
       (apply max)))

(defn solution-1 [] (winner 452 70784))
