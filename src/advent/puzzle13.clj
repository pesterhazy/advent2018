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

(defn transform-cart [graph [[x y] cart-symbol]]
  (let [[new-x new-y]
        (case cart-symbol
          \^ [x (dec y)]
          \v [x (inc y)]
          \< [(dec x) y]
          \> [(inc x) y])
        point (point-in graph new-x new-y)
        ky [cart-symbol point]
        new-symbol (case ky
                     [\> \\] \v
                     [\> \/] \^
                     [\< \\] \^
                     [\< \/] \v
                     [\^ \\] \<
                     [\^ \/] \>
                     [\v \\] \>
                     [\v \/] \<
                     (do
                       (assert (#{\- \|} point) (str "Unexpected: " ky))
                       cart-symbol))]
    [[x y] new-symbol]))

(defn tick [graph carts]
  (->> carts
       (map (partial transform-cart graph))
       (into (sorted-map))))

(defn print-graph [graph carts]
  (doseq [[y line] (map-indexed vector graph)]
    (println (apply str (for [[x point] (map-indexed vector line)]
                          (or (carts [x y])
                              (cart->line point point)))))))

(defn solution-1 []
  (let [graph (read-sample)
        carts (find-carts graph)]
    (print-graph graph (tick graph carts))))
