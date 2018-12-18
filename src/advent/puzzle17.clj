(ns advent.puzzle17
  (:import java.util.HashSet))

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

(defn is-clay? [walls [y x]]
  (some (fn [{:keys [k1 v1 v2-start v2-end]}]
          (if (= k1 "x")
            (and (= x v1) (<= v2-start y v2-end))
            (and (= y v1) (<= v2-start x v2-end))))
        walls))

;; h/t
;; https://www.reddit.com/r/adventofcode/comments/a6wpup/2018_day_17_solutions/ebyq6mj/

(defn solution-1 []
  (let [walls (->> (read-input)
                   (mapv parse))
        puzzle-min-y (min-y walls)
        puzzle-max-y (max-y walls)
        clay? #(is-clay? walls %)
        settled (HashSet.)
        flowing (HashSet.)
        visit (fn visit [[y x :as yx] dir]
                (.add flowing yx)
                (let [below [(inc y) x]]
                  (when (and (not (clay? below))
                             (not (.contains flowing below))
                             (<= 1 (first below) puzzle-max-y))
                    (visit below :down))
                  (if (and (not (clay? below))
                           (not (.contains settled below)))
                    false
                    (let [left [y (dec x)]
                          right [y (inc x)]
                          left-filled (or (clay? left)
                                          (and (not (.contains flowing left))
                                               (visit left :left)))
                          right-filled (or (clay? right)
                                           (and (not (.contains flowing right))
                                                (visit right :right)))]
                      (cond
                        (and (= :down dir)
                             left-filled
                             right-filled)
                        (do
                          (.add settled yx)
                          (loop [ptr left]
                            (when (.contains flowing ptr)
                              (.add settled ptr)
                              (recur [(first ptr) (dec (second ptr))])))
                          (loop [ptr right]
                            (when (.contains flowing ptr)
                              (.add settled ptr)
                              (recur [(first ptr) (inc (second ptr))])))
                          false)

                        (= :left dir)
                        (or left-filled (clay? left))

                        (= :right dir)
                        (or right-filled (clay? right))

                        :else
                        false)))))]
    (try
      (visit [0 500] :down)
      (.addAll flowing settled)
      ;; part1 part2
      [(->> flowing
            (filter (fn [[y _]]
                      (<= puzzle-min-y y puzzle-max-y)))
            count)
       (->> settled
            (filter (fn [[y _]]
                      (<= puzzle-min-y y puzzle-max-y)))
            count)]
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
