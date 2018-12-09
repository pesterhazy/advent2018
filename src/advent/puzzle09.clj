(ns advent.puzzle09)

(defprotocol IMarbleList
  (insert-1 [mlist x]
            "Insert a marble after the 1st element. New marble becomes current")
  (backshift
    [mlist backshift-pos]
    "Removes a marble backshift-pos marble before current.
Marble after remove marble comes current")
  (nth-ccw [mlist n] "Returns the marble n places before the current"))

;; -----------------
;; Implementation 1: vector

(extend-type clojure.lang.PersistentVector
  IMarbleList
    (insert-1 [mlist x]
      (into [x]
            (->> mlist
                 cycle
                 (drop 2)
                 (take (count mlist)))))
    (backshift [mlist backshift-pos]
      (->> mlist
           cycle
           (drop (- (count mlist) (dec backshift-pos)))
           (take (dec (count mlist)))
           vec))
    (nth-ccw [mlist n] (nth mlist (- (count mlist) n))))

(defn empty-mlist [] [])

;; -----------------
;; Implementation 2

;; TODO: what if no space between floats?

(defn between
  [a b]
  (if (< a b)
    (let [v (* 0.5 (+ a b))]
      (if (< a v b) v (throw (ex-info "Invariant failed" {:a a, :b b, :v v}))))
    (let [v (+ a 1.0)]
      (if (< b a v)
        v
        (throw (ex-info "Invariant failed" {:a a, :b b, :v v}))))))

(defn find-pos
  [pos ps]
  (case (count ps)
    0 1.0
    (->> ps
         (concat ps)
         (partition-all 3 1)
         (some (fn [[a b c]]
                 (cond (nil? b) (+ a 1.0)
                       (nil? c) (+ b 1.0)
                       :else (when (= a pos) (between b c))))))))

(defrecord FastMarbleList [m current-k]
  IMarbleList
    (insert-1 [_ x]
      (let [new-k (find-pos current-k (keys m))]
        (->FastMarbleList (assoc m new-k x) new-k)))
    (backshift [_ backshift-pos] m)
    (nth-ccw [_ n]
      (let [ks (keys m)] (mod (- (.indexOf ks current-k) n) (count ks)))))

(defn empty-fast-mlist [] (->FastMarbleList (sorted-map) nil))

;; -----------------

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
    (second (reduce turn [(empty-fast-mlist) nil] marbles))))

(defn winner
  [n-players n-marbles]
  (->> (range 0 (inc n-marbles))
       (simulate {:backshift-pos 7, :bingo 23, :n-players n-players})
       vals
       (apply max)))

(defn solution-1 [] (winner 452 70784))
