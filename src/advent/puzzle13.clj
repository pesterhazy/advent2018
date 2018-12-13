(ns advent.puzzle13)

(defn read-sample
  []
  (with-open [f (-> "13/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn point-in [graph x y]
  (nth (nth graph y) x))

(def cart-symbol? #{\^ \> \v \<})

(defn find-carts [graph]
  (for [[y line] (map-indexed vector graph),
        [x point] (map-indexed vector line)
        :when (cart-symbol? point)]
    [x y point]))
