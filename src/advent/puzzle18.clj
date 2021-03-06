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

(defn value [state]
  (let [fs (->> state
                (mapcat seq)
                frequencies)]
    (* (fs \|) (fs \#))))

(defn solution-1 []
  (-> (->> (read-input)
           (iterate turn))
      (nth 10)
      value))

(def goal 1000000000)

(defn solution-2 []
  (let [[start length states]
        (->> (read-input)
             (iterate turn)
             (map-indexed vector)
             (reduce (fn [[acc-map acc-states] [idx state]]
                       (when (zero? (mod idx 100))
                         (println idx))
                       (let [s (str/join state)]
                         (if (acc-map s)
                           (reduced [(acc-map s)
                                     (- idx (acc-map s))
                                     acc-states])
                           [(assoc acc-map s idx)
                            (assoc acc-states idx state)])))
                     [{} {}]))]
    (value (states (+ start (mod (- goal start) length))))))

;; REPL stuff; ignore.

(defn again []
  (solution-2))

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
