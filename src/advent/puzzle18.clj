(ns advent.puzzle18
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "18/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "18/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def directions
  (for [x [-1 0 1] y [-1 0 1] :when (or (not= x 0) (not= y 0))] [y x]))

(defn pos+ [& vs] (apply mapv + vs))

(defn neighbors [state yx]
  (->> directions
       (map (partial pos+ yx))
       (keep (partial get-in state))
       frequencies))

(defn turn-1 [state yx]
  (let [square (get-in state yx)
        nbs (neighbors state yx)]
    (case square
      \. (if (>= (nbs \| 0) 3)
           \|
           square)
      \| (if (>= (nbs \# 0) 3)
           \#
           square)
      \# (if (and (>= (nbs \# 0) 1)
                  (>= (nbs \| 0) 1))
           square
           \.))))

(defn turn [state]
  (mapv (fn [y]
          (->> (range (count (get state y)))
               (map (fn [x]
                      (turn-1 state [y x])))
               str/join))
        (range (count state))))

;; REPL stuff; ignore.

(defn again []
  (->> (read-sample)
       (iterate turn)
       (take 11)
       (run! (fn [generation]
               (run! println generation)
               (println)))))

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
