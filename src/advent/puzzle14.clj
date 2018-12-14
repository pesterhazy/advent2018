(ns advent.puzzle14
  (:require [clojure.string :as str]))

(def initial-state {:positions [0 1], :nums [3 7]})

(defn ->recipes
  [a b]
  (let [sum (+ a b)]
    (->> (str sum)
         (map (fn [c] (- (int c) 48))))))

(defn next-state
  [init-state]
  (let [new-nums (apply ->recipes
                        (map (:nums init-state) (:positions init-state)))
        {:keys [nums], :as state*} (update init-state :nums into new-nums)]
    (update state*
            :positions
            (fn [positions]
              (->> positions
                   (mapv (fn [position]
                           (mod (+ position (nums position) 1)
                                (count nums)))))))))

(defn generations [] (iterate next-state initial-state))

(defn recipes []
  (let [gens (->> (generations)
                  (map :nums)
                  (partition 2 1))
        fst (ffirst gens)]
    (concat fst
            (mapcat (fn [[prev cur]]
                      (subvec cur (count prev)))
                    gens))))

(defn solution-1
  [n]
  (let [state (->> (generations)
                   (drop-while (fn [state] (< (count (:nums state)) (+ n 10))))
                   first)]
    (->> state
         :nums
         (drop n)
         (take 10)
         (str/join))))

(defn solution-2
  [^String needle]
  (->> (recipes)
       (partition (count needle) 1)
       (map-indexed vector)
       (drop-while (fn [[_ nums]]
                     (not= needle (str/join nums))))
       ffirst))
