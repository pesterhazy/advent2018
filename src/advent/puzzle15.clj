(ns advent.puzzle15
  (:require [clojure.string :as str])
  (:import java.util.LinkedList
           java.util.HashMap))

(defn read-input
  []
  (with-open [f (-> "15/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-sample
  []
  (with-open [f (-> "15/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn parse [lines]
  (mapv vec lines))

(def terrain? #{\# \.})

(defn extract [grid]
  (let [m
        (->> grid
             (map-indexed vector)
             (reduce (fn [[acc-v acc-units] [y row]]
                       (let [[new-row new-units]
                             (->> row
                                  (map-indexed vector)
                                  (reduce (fn [[acc-v acc-units] [x c]]
                                            [(conj acc-v (get terrain? c \.))
                                             (case c
                                               \E (conj acc-units {:x x
                                                                   :y y
                                                                   :type :elf})
                                               \G (conj acc-units {:x x
                                                                   :y y
                                                                   :type :goblin})
                                               acc-units)])
                                          [[] acc-units]))]
                         [(conj acc-v new-row)
                          new-units]))
                     [[] []])
             (zipmap [:grid :units]))]
    (update m :units (fn [units]
                       (->> units
                            (map-indexed (fn [id unit]
                                           [id (assoc unit :id id)]))
                            (into {}))))))

(def tstate (-> (read-sample) parse extract))
(def tgrid (:grid tstate))

(defn print-grid [grid]
  (run! println (map str/join grid)))

(defn highlight [grid [x y]]
  (assoc-in grid [x y] \x))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn neighbors [grid [x y]]
  (keep (fn [[dx dy]]
          (let [xy [(+ x dx) (+ y dy)]]
            (when (= \. (get-in grid xy))
              xy)))
        directions))

(defn find-path
  "Breath-first search"
  [grid start end]
  (let [frontier (LinkedList.)
        came-from (HashMap.)]
    (.add frontier start)
    (.put came-from start nil)
    (loop []
      (when-not (.isEmpty frontier)
        (let [current (.remove frontier)]
          (when-not (= end current)
            (doseq [nxt (neighbors grid current)]
              (when-not (.containsValue came-from nxt)
                (.add frontier nxt)
                (.put came-from nxt current)))
            (recur)))))
    (loop [result (list end)]
      (if (= start (first result))
        result
        (recur (conj result (.get came-from (first result))))))))

(comment
  (print-grid (reduce highlight tgrid (find-path tgrid [2 3] [2 5]))))
