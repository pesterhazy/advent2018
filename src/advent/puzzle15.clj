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

(defn read-sample2
  []
  (with-open [f (-> "15/sample2.txt"
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

(def t2state (-> (read-sample2) parse extract))
(def t2grid (:grid t2state))

(def istate (-> (read-input) parse extract))
(def igrid (:grid istate))

(defn print-grid [grid]
  (run! println (map str/join grid)))

(defn highlight [grid [x y]]
  (assoc-in grid [y x] \x))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn point-in [grid [x y]]
  (get-in grid [y x]))

(defn neighbors [grid [x y]]
  (keep (fn [[dx dy]]
          (let [xy [(+ x dx) (+ y dy)]]
            (when (= \. (point-in grid xy))
              xy)))
        directions))

;; https://www.redblobgames.com/pathfinding/a-star/introduction.html

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
              (when-not (.containsKey came-from nxt)
                (.add frontier nxt)
                (.put came-from nxt current)))
            (recur)))))
    (when (.containsKey came-from end)
      (loop [result (list end)]
        (if (= start (first result))
          result
          (recur (conj result (.get came-from (first result)))))))))

(defn test1 []
  (print-grid (reduce highlight tgrid (find-path tgrid [2 3] [2 5]))))

(defn test2 []
  (print-grid (reduce highlight t2grid (find-path t2grid [2 3] [2 5]))))

(defn test3 []
  (print-grid (-> igrid (highlight [1 10])))
  (println)
  (print-grid (reduce highlight igrid (find-path igrid [1 10] [10 10]))))

(comment
  )
