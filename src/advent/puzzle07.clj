(ns advent.puzzle07
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "7/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "7/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def regex #"Step (\S+) must be finished before step (\S+) can begin\.")

(defn parse
  [line]
  (->> (re-matches regex line)
       rest
       vec))

(defn ->deps [pairs]
  (reduce (fn [acc [b a]]
            (-> acc
                (update a (fn [xs] (conj (or xs #{}) b)))
                ;; make sure b is present in map as well
                (update b (fn [xs] (or xs #{})))))
          (sorted-map)
          pairs))

(defn walk [m]
  (loop [done #{}
         acc []]
    (let [nxt (->> m
                   (some (fn [[k v]]
                           (when (and (not (done k))
                                      (set/subset? v done))
                             k))))]
      (if nxt
        (recur (conj done nxt) (conj acc nxt))
        (str/join acc)))))

{0 [3 "C"]}

(defn solution-1 []
  (->> (read-input)
       (map parse)
       ->deps
       walk))

(defn cost [task]
  (-> task (.charAt 0) int (- (int \A)) inc))

(defn next-time [workers]
  (->> workers
       vals
       (filter identity)
       (map first)
       (apply min)))

(defn walk-2 [m n-workers delay]
  (loop [workers (->> (range n-workers) (map (fn [k] [k nil])) (into {}))
         now 0
         done #{}
         acc []]
    (if-let [[ready-id ready-task] (some (fn [[id [busy-until task]]]
                                           (when (and busy-until
                                                      (>= now busy-until))
                                             [id task]))
                                         workers)]
      ;; worker finished
      (recur (assoc workers ready-id nil)
             now
             (conj done ready-task)
             (conj acc ready-task))
      (let [nxt-tasks (->> m
                           (keep (fn [[k v]]
                                   (when (and (not (done k))
                                              (set/subset? v done))
                                     k))))
            available? (->> workers vals (map second) set complement)
            nxt (->> nxt-tasks (filter available?) first)]
        (cond
          ;; new task available
          nxt
          (let [worker-id (some (fn [[id task-vec]]
                                  (when (nil? task-vec) id))
                                workers)]
            (if worker-id
              ;; found an idle worker
              (recur (assoc workers worker-id [(+ now delay (cost nxt))
                                               nxt])
                     now
                     done
                     acc)
              ;; no idle worker found
              (recur workers (next-time workers) done acc)))
          ;; new tasks available but all in flight
          (seq nxt-tasks)
          (recur workers (next-time workers) done acc)
          ;; no new tasks available
          :else
          now
          #_(str/join acc))))))
