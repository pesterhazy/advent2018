(ns advent.puzzle17
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "17/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "17/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn parse [line]
  (let [matches (re-matches #"([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)" line)]
    (assert matches)
    (-> (->> (rest matches)
             (zipmap [:k1 :v1 :k2 :v2-start :v2-end]))
        (update :v1 #(Long/parseLong %))
        (update :v2-start #(Long/parseLong %))
        (update :v2-end #(Long/parseLong %)))))

(def origin [0 500])

(defn max-y [walls]
  (->> walls
       (map (fn [{:keys [k1 v1 v2-start v2-end]}]
              (if (= k1 "x")
                (max v2-start v2-end)
                v1)))
       (apply max)))

(defn min-y [walls]
  (->> walls
       (map (fn [{:keys [k1 v1 v2-start v2-end]}]
              (if (= k1 "x")
                (min v2-start v2-end)
                v1)))
       (apply min)))

(defn free? [walls [y x :as yx]]
  (when (not (some (fn [{:keys [k1 v1 v2-start v2-end]}]
                     (if (= k1 "x")
                       (and (= x v1) (<= v2-start y v2-end))
                       (and (= y v1) (<= v2-start x v2-end))))
                   walls))
    yx))

(defn solution-1 []
  (let [walls (->> (read-input)
                   (mapv parse))
        puzzle-min-y (min-y walls)
        puzzle-max-y (max-y walls)
        !count (volatile! 0)
        !visited (volatile! #{})
        !settled (volatile! {})
        can-go (fn [[y x :as yx]]
                 (when (and (free? walls [(inc y) x])
                            (not (@!visited yx)))
                   yx))
        visit (fn visit
                [[y x :as yx]]
                (let [settled (if (not (<= puzzle-min-y y puzzle-max-y))
                                false
                                (let [_ (when-not (< (vswap! !count inc) 1000000)
                                          (throw (ex-info "Exceeded max" {:exceeded true})))
                                      _ (vswap! !visited conj yx)
                                      down-settled (cond
                                                     (not (free? walls [(inc y) x]))
                                                     true
                                                     (@!visited [(inc y) x])
                                                     (do
                                                       (assert (contains? @!settled [(inc y) x]))
                                                       (@!settled [(inc y) x]))
                                                     :else
                                                     (visit [(inc y) x]))]
                                  (if down-settled
                                    (let [left-settled (if (can-go [y (dec x)])
                                                         (visit [y (dec x)])
                                                         true)
                                          right-settled (if (can-go [y (inc x)])
                                                          (visit [y (inc x)])
                                                          true)]
                                      (and left-settled right-settled))
                                    false)))]
                  (vswap! !settled assoc yx settled)
                  settled))]
    (try
      (visit [puzzle-min-y 500])
      (->> @!visited
           (filter (fn [[y _]]
                     (<= puzzle-min-y y puzzle-max-y)))
           count)
      (catch Exception e
        (if (-> e ex-data :exceeded)
          (do
            (prn [:excceeded])
            nil)
          (throw e))))))

;; REPL stuff; ignore.

(defn again []
  (solution-1))

(defonce bq (java.util.concurrent.LinkedBlockingQueue.))

(defn wait
  "Wait on blocking queue for invocations. When new
  value becomes available, remove and re-run fun.

  User can end the loop by pressing return.

  Useful for calling a test function in a terminal REPL whenever
  the namespace is re-evaluated from a different thread, such
  as an nREPL connection. e.g.:

  (wait solution-2)"
  [fun]
  (.clear bq)
  (prn (fun))
  (loop []
    (if (.poll bq)
      (do
        (println (apply str (repeat 60 "*")))
        (prn (fun))
        (recur))
      (let [n (.available System/in)]
        (if (> n 0)
          (read-line)
          (do
            (Thread/sleep 100)
            (recur))))))
  nil)

;; When ns gets reloaded via nREPL, trigger new call
;; of function passed to wait

(.add bq true)
