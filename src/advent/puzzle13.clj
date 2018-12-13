(ns advent.puzzle13)

(defn read-sample
  []
  (with-open [f (-> "13/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn point-in [graph x y]
  (nth (nth graph y) x))

(def cart->line {\^ \|
                 \> \-
                 \v \-
                 \< \|})

(defn find-carts [graph]
  (->> (for [[y line] (map-indexed vector graph),
             [x point] (map-indexed vector line)
             :when (cart->line point)]
         [[x y] point])
       (into (sorted-map))))

(defn tick [graph carts]
  carts)

(defn print-graph [graph carts]
  (doseq [[y line] (map-indexed vector graph)]
    (println (apply str (for [[x point] (map-indexed vector line)]
                          (or (carts [x y])
                              (cart->line point point)))))))

(defn solution-1 []
  (let [graph (read-sample)
        carts (find-carts graph)]
    (print-graph (tick graph carts))))
