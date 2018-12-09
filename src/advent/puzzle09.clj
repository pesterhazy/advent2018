(ns advent.puzzle09)

(defn empty-mlist [] [])

(defn nth-ccw
  "Returns the marble n places before the current"
  [mlist n]
  (nth mlist (- (count mlist) n)))

(defn insert-1
  "Insert a marble before the 2nd element. New marble becomes current"
  [mlist x]
  (into [x]
        (->> mlist
             cycle
             (drop 2)
             (take (count mlist)))))

(defn backshift
  "Removes a marble backshift-pos marble before current. Marble after remove
  marble comes current"
  [mlist backshift-pos]
  (->> mlist
       cycle
       (drop (- (count mlist) (dec backshift-pos)))
       (take (dec (count mlist)))
       vec))

(defn simulate
  "Takes a sequence of marbles, returns score"
  [{:keys [n-players backshift-pos bingo]} marbles]
  (let [turn (fn [[mlist score] x]
               (if (and (pos? x) (zero? (mod x bingo)))
                 (let [player (inc (mod (dec x) n-players))]
                   [(backshift mlist backshift-pos)
                    (update score
                            player
                            (fn [n]
                              (+ (or n 0) x (nth-ccw mlist backshift-pos))))])
                 [(insert-1 mlist x) score]))]
    (second (reduce turn [(empty-mlist) nil] marbles))))

(defn winner
  [n-players n-marbles]
  (->> (range 0 (inc n-marbles))
       (simulate {:backshift-pos 7, :bingo 23, :n-players n-players})
       vals
       (apply max)))

(defn solution-1 [] (winner 452 70784))
