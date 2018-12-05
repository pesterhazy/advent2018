(ns advent.puzzle05
  (:require [clojure.string :as str]))

(def sample-input "dabAcCaCBAcCcaDA")

(defn polar?
  [a b]
  (and (not= a b) (= (Character/toUpperCase a) (Character/toUpperCase b))))

(defn find-pred
  [pred xs start-idx reverse?]
  (->> (if reverse?
         (range (dec start-idx) -1 -1)
         (range (inc start-idx) (count xs)))
       (some (fn [idx] (when (pred (nth xs idx)) idx)))))

(defn scan
  [xs]
  (->> (loop [xs (vec xs)
              idx 0]
         (cond
           (>= idx (dec (count xs))) xs
           (nil? (nth xs idx)) (recur xs (inc idx))
           :else (let [left (find-pred identity xs idx true)]
                   (if (and left (polar? (nth xs left) (nth xs idx)))
                     (recur (assoc xs
                                   idx nil
                                   left nil)
                            idx)
                     (let [right (find-pred identity xs idx false)]
                       (if (and right (polar? (nth xs idx) (nth xs right)))
                         (recur (assoc xs
                                       idx nil
                                       right nil)
                                idx)
                         (recur xs (inc idx))))))))
       (filter identity)
       count))

(defn solution-1 []
  (scan (vec (str/trim (slurp "5/input.txt")))))

(defn solution-2 []
  (let [input (vec (str/trim (slurp "5/input.txt")))]
    (->> (map char (range 97 123))
         (map (fn [ch]
                (prn ch)
                (->> input
                     (filterv #(and (not= ch %)
                                    (not= (Character/toUpperCase ch) %)))
                     (scan))))
         (apply min))))
