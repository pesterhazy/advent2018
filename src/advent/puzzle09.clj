(ns advent.puzzle09)

(defn nth-ccw [xs n] (nth xs (- (count xs) n)))

(defn game
  [{:keys [n-players backshift bingo]} marbles]
  (let [turn (fn [[xs score] x]
               (if (zero? (mod x bingo))
                 (let [player (inc (mod (dec x) n-players))]
                   [(->> xs
                         cycle
                         (drop (- (count xs) (dec backshift)))
                         (take (dec (count xs)))
                         vec)
                    (update score
                            player
                            (fn [n] (+ (or n 0) x (nth-ccw xs backshift))))])
                 [(into [x]
                        (->> xs
                             cycle
                             (drop 2)
                             (take (count xs)))) score]))]
    (second (reduce turn [[1] nil] marbles))))

(defn solution-1
  []
  (->> (range 1 (inc 70784))
       (game {:backshift 7, :bingo 23, :n-players 452})
       vals
       (apply max)))
