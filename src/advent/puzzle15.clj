(ns advent.puzzle15
  (:require [clojure.string :as str])
  (:import (java.util LinkedList Comparator)
           java.util.PriorityQueue
           java.util.HashMap))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

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

(defn read-sample3
  []
  (with-open [f (-> "15/sample3.txt"
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
                                               \E (conj acc-units {:yx [y x]
                                                                   :type :elf})
                                               \G (conj acc-units {:yx [y x]
                                                                   :type :goblin})
                                               acc-units)])
                                          [[] acc-units]))]
                         [(conj acc-v new-row)
                          new-units]))
                     [[] []])
             (zipmap [:base-grid :units]))]
    (update m :units (fn [units]
                       (->> units
                            (map-indexed (fn [id unit]
                                           [id (assoc unit :id id :hp 200)]))
                            (into {}))))))

(def t-state (-> (read-sample) parse extract))
(def t-base-grid (:base-grid t-state))

(def t2-state (-> (read-sample2) parse extract))
(def t2-base-grid (:base-grid t2-state))

(def i-state (-> (read-input) parse extract))
(def i-base-grid (:base-grid i-state))

(defn print-grid [grid]
  (doseq [line grid]
    (println (str/join (map (fn [c]
                              (if (= \x c)
                                "\u001B[32m•\u001B[0m"
                                c)) line)))))

(defn decorate [{:keys [base-grid units]}]
  (->> units
       vals
       (reduce (fn [grid unit]
                 (assoc-in grid (:yx unit) (case (:type unit)
                                             :elf "E"
                                             :goblin "G")))
               base-grid)))

(defn highlight [grid [y x]]
  (assoc-in grid [y x] \x))

(defn print-state [state & squares]
  (print-grid (reduce highlight (decorate state) squares))
  (prn (->> state
            :units
            vals
            (sort-by :id)
            (map (juxt :id :hp))))
  (println))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn neighbors [grid [^long y ^long x]]
  (keep (fn [[^long dx ^long dy]]
          (let [yx [(+ y dy) (+ x dx)]]
            (when (= \. (get-in grid yx))
              yx)))
        directions))

;; https://www.redblobgames.com/pathfinding/a-star/introduction.html

(defn find-path
  "Breadth-first search"
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

(defn priority-queue
  []
  (let [cmpr ^Comparator (comparator
                          (fn [a b]
                            (< ^long (first a) ^long (first b))))]
    (PriorityQueue. cmpr)))

(defn manhattan ^long [[^long y1 ^long x1] [^long y2 ^long x2]]
  (+ (Math/abs (- y1 y2))
     (Math/abs (- x1 x2))))

(defn find-path*
  "A*"
  [neighbors-fn start end]
  (let [frontier (PriorityQueue.)
        came-from (HashMap.)
        cost-so-far (HashMap.)]
    (.add frontier [0 start])
    (.put came-from start nil)
    (.put cost-so-far start 0)
    (loop []
      (when-not (.isEmpty frontier)
        (let [current (second (.remove frontier))]
          (when-not (= end current)
            (doseq [nxt (neighbors-fn current)]
              (let [new-cost (inc ^long (.get cost-so-far current))]
                (when (or (not (.containsKey cost-so-far nxt))
                          (< new-cost ^long (.get cost-so-far nxt)))
                  (.put cost-so-far nxt new-cost)
                  (.add frontier [(+ new-cost (manhattan end nxt)) nxt])
                  (.put came-from nxt current))))
            (recur)))))
    (when (.containsKey came-from end)
      (loop [result (list end)]
        (if (= start (first result))
          result
          (recur (conj result (.get came-from (first result)))))))))

(defn targets [state unit]
  (assert unit)
  (->> state
       :units
       vals
       (filter (fn [unit*] (and (not= (:id unit) (:id unit*))
                                (not= (:type unit) (:type unit*)))))))

(defn fights
  "Returns map of [yx-attack-square unit]"
  [state unit]
  (let [inhabited-grid (-> (decorate state)
                           (assoc-in (:yx unit) \.))]
    (->> (targets state unit)
         (mapcat (fn [target]
                   (map vector (neighbors inhabited-grid (:yx target))
                        (repeat target))))
         (into {}))))

(defn move [state unit in-range]
  (let [inhabited-grid (decorate state)
        chosen (->> in-range
                    (keep (fn [[yx _]]
                            (when-let [distance (some-> (find-path* #(neighbors inhabited-grid %)
                                                                    (:yx unit)
                                                                    yx)
                                                        count)]
                              [yx distance])))
                    (reduce (fn [[^long acc-minimum acc-set :as acc]
                                 [yx ^long distance]]
                              (cond (< distance acc-minimum)
                                    [distance #{yx}]
                                    (= distance acc-minimum)
                                    [distance (conj acc-set yx)]
                                    :else
                                    acc))
                            [Long/MAX_VALUE #{}])
                    second
                    sort
                    first)]
    (if-not chosen
      state
      ;; when there's a tie, pick direction in reading order
      (let [candidates (->> (neighbors inhabited-grid (:yx unit))
                            (keep (fn [yx]
                                    (when-let [distance (some-> (find-path* #(neighbors inhabited-grid %)
                                                                            chosen
                                                                            yx)
                                                                count)]
                                      [distance yx])))
                            sort)]
        (assert (seq candidates))
        (update-in state [:units (:id unit)]
                   (fn [unit]
                     (assoc unit :yx (-> candidates first second))))))))

(defn attack [state id]
  (let [candidates (set (neighbors (:base-grid state) (get-in state [:units id :yx])))
        ts (->> (targets state (get-in state [:units id]))
                (filter (comp candidates :yx))
                (sort-by (juxt :hp :yx)))]
    (if (empty? ts)
      state
      (do
        (prn [:attack ts])
        (update-in state [:units (:id (first ts)) :hp]
                   (fn [hp] (- hp 3)))))))

(defn turn [state id]
  (let [in-range (fights state (get-in state [:units id]))
        can-attack? (in-range (get-in state [:units id :yx]))
        state* (if can-attack?
                 state ;; skip if already in range
                 (move state (get-in state [:units id]) in-range))]
    (attack state* id)))

(defn round [state]
  (->> state
       :units
       vals
       (map :id)
       sort
       (reduce (fn [state id]
                 (turn state id))
               state)))

(defn test1 []
  (print-grid (reduce highlight t-base-grid (find-path* t-base-grid [2 3] [2 5]))))

(defn test2 []
  (print-grid (reduce highlight t2-base-grid (find-path t2-base-grid [2 3] [2 5]))))

(defn test3 [find-path-fn]
  (let [path (time (find-path-fn i-base-grid [10 1] [19 30]))]
    (print-grid (reduce highlight i-base-grid path))
    (prn {:path-length (count path)})))

(defn test4 []
  (let [generations (iterate round t-state)]
    (->> generations
         (take 3)
         (run! print-state))))

(defn test5 []
  (let [generations (iterate round (-> (read-sample3) parse extract))]
    (->> generations
         (take 8)
         (map-indexed vector)
         (run! (fn [[idx state]]
                 (println)
                 (println "***" idx "***")
                 (print-state state))))))
