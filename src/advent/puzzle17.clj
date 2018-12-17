(ns advent.puzzle17
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "17/sample.txt"
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

(defn free? [walls [y x :as yx]]
  (when (not (some (fn [{:keys [k1 v1 v2-start v2-end]}]
                     (if (= k1 "x")
                       (and (= x v1) (<= v2-start y v2-end))
                       (and (= y v1) (<= v2-start x v2-end))))
                   walls))
    yx))

(defn again []
  (let [walls (->> (read-sample)
                   (mapv parse))
        my (max-y walls)
        !count (volatile! 0)
        !visited (volatile! #{})
        can-go (fn [[y x :as yx]]
                 (when (and (free? walls [(inc y) x])
                            (not (@!visited yx)))
                   yx))
        visit (fn visit
                [[y x :as yx]]
                (when-not (< (vswap! !count inc) 40)
                  (throw (ex-info "Exceeded max" {:exceeded true})))
                (vswap! !visited conj yx)

                (if (can-go [(inc y) x])
                  (do
                    (prn [yx :down true])
                    (visit [(inc y) x]))
                  (do
                    (if (can-go [y (dec x)])
                      (do
                        (prn [yx :left true])
                        (visit [y (dec x)]))
                      (do
                        (prn [yx :left false])))
                    (if (can-go [y (inc x)])
                      (do
                        (prn [yx :right true])
                        (visit [y (inc x)]))
                      (do
                        (prn [yx :right false])))
                    (prn [yx :down false]))))]
    (try
      (visit origin)
      (catch Exception e
        (if (-> e ex-data :exceeded)
          (do
            (prn [:excceeded])
            nil)
          (throw e))))))


;; REPL stuff; ignore.

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
