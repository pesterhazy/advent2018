(ns advent.puzzle03)

(def sample-input "#1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2")

(def regex #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
(defn parse [line]
  (zipmap [:id :x :y :width :height] (rest (re-matches regex line))))

(defn read-sample-input
  []
  (->> sample-input
       java.io.StringReader.
       clojure.java.io/reader
       line-seq
       (map clojure.string/trim)))
