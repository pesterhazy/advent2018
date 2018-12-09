(ns advent.puzzle09
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defprotocol IMarbleList
  (insert-1 [mlist x]
    "Insert a marble after the 1st element. New marble becomes current")
  (backshift
    [mlist n]
    "Removes a marble n marble before current.
Marble after the one just removed comes current")
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
  (backshift [mlist ^long n]
    (->> mlist
         cycle
         (drop (- (count mlist) (dec n)))
         (take (dec (count mlist)))
         vec))
  (nth-ccw [mlist n] (nth mlist (- ^int (count mlist) ^long n))))

(defn empty-mlist [] [])

;; -----------------
;; Implementation 2

(defn between
  [^double a ^double b]
  (cond
    (< a b)
    (let [v (* 0.5 (+ a b))]
      (if (< a v b) v (throw (ex-info "Invariant failed" {:a a, :b b, :v v}))))
    (> a b)
    (let [v (+ a 1.0)]
      (if (< b a v)
        v
        (throw (ex-info "Invariant failed" {:a a, :b b, :v v}))))
    :else
    (+ a 1.0)))

(defn find-pos
  [pos ps]
  (case (count ps)
    0 1.0
    (->> ps
         (concat ps)
         (partition-all 3 1)
         (some (fn [[^double a ^double b ^double c]]
                 (cond (nil? b) (+ a 1.0)
                       (nil? c) (+ b 1.0)
                       :else (when (= a pos) (between b c))))))))

(defn find-pos*
  [pos ps]
  (if (empty? ps)
    1.0
    (loop [[x & rst :as xs] ps]
      (cond
        (= x pos)
        (let [wrap-around (concat rst ps ps)]
          (between (first wrap-around) (second wrap-around)))
        rst
        (recur rst)
        :else
        nil))))

(defrecord FastMarbleList [m current-k]
  IMarbleList
  (insert-1 [_ x]
    (let [new-k (find-pos* current-k (keys m))]
      (->FastMarbleList (assoc m new-k x) new-k)))
  (backshift [_ n]
    (let [ks (vec (keys m))
          idx ^long (mod ^long (- ^int (.indexOf ^clojure.lang.PersistentVector ks current-k) ^long n) ^long (count ks))
          idx-after (mod (inc ^long idx) (count ks))]
      (->FastMarbleList (dissoc m (nth ks idx))
                        (nth ks idx-after))))
  (nth-ccw [_ n]
    (let [ks (vec (keys m))]
      (get m (nth ks (mod (- (.indexOf ^clojure.lang.PersistentVector ks current-k) ^long n) (count ks)))))))

(defn print-fast-mlist
  [{:keys [m current-k]}]
  (->> m
       (map (fn [[k v]]
              (if (= k current-k)
                (str "(" v ")")
                (str v))))
       (str/join " ")))

(defn empty-fast-mlist [] (->FastMarbleList (sorted-map) nil))

;; -----------------

(defn simulate
  "Takes a sequence of marbles, returns score"
  [{:keys [^long n-players ^long backshift-pos ^long bingo]} marbles]
  (let [turn (fn [[mlist score] ^long x]
               (when (= 0 (mod x 100))
                 (println "..." x))
               (if (and (pos? x) (zero? ^long (mod x bingo)))
                 (let [player (inc ^long (mod (dec x) n-players))]
                   [(backshift mlist backshift-pos)
                    (update score
                            player
                            (fn [n]
                              (if n
                                (+ ^long n x ^long (nth-ccw mlist backshift-pos))
                                (+ x ^long (nth-ccw mlist backshift-pos)))))])
                 [(insert-1 mlist x) score]))]
    (second (reduce turn [(empty-fast-mlist) nil] marbles))))

(defn winner
  [n-players ^long n-marbles]
  (->> (range 0 (inc n-marbles))
       (simulate {:backshift-pos 7, :bingo 23, :n-players n-players})
       vals
       (apply max)))

(defn solution-1 [] (time (winner 452 70784)))
