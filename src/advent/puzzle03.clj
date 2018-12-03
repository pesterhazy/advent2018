(ns advent.puzzle03)

(set! *warn-on-reflection* true)

(def sample-input "#1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2")

(def regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse [line]
  (->> (re-matches regex line)
       rest
       (map #(Long/parseLong %))
       (zipmap [:id :left :top :width :height])))

(defn read-sample-input
  []
  (->> sample-input
       java.io.StringReader.
       clojure.java.io/reader
       line-seq
       (map clojure.string/trim)))

(defn read-input
  []
  (with-open [f (-> "3/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def canvas-width 1000)
(def canvas-height 1000)

(defn make-canvas []
  (make-array Integer/TYPE canvas-width canvas-height))

(defn paint! [canvas {:keys [left top width height]}]
  (doseq [y (range top (+ top height))
          x (range left (+ left width))]
    (aset-int canvas x y (inc (aget canvas x y)))))

(defn overlap [canvas]
  (->> (for [y (range canvas-height)
             x (range canvas-width)]
         (if (> (aget canvas x y) 1) 1 0))
       (reduce +)))

(defn solution-1 [lines]
  (let [canvas (make-canvas)
        claims (->> lines
                    (map parse))]
    (doseq [claim claims]
      (paint! canvas claim))
    (overlap canvas)))
