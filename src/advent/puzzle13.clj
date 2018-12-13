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

(def directions [\^ \> \v \<])

(defn find-carts [graph]
  (->> (for [[y line] (map-indexed vector graph),
             [x point] (map-indexed vector line)
             :when (cart->line point)]
         [[x y] {:sym point
                 :n-turns 0}])
       (into (sorted-map))))

(defn rotate [sym nturns]
  (case (mod nturns 3)
    0 (nth directions (mod (dec (.indexOf directions sym)) 4))
    1 sym
    2 (nth directions (mod (inc (.indexOf directions sym)) 4))))

(defn transform-cart [graph [[x y] {:keys [n-turns sym] :as cart}]]
  (let [[new-x new-y]
        (case sym
          \^ [x (dec y)]
          \v [x (inc y)]
          \< [(dec x) y]
          \> [(inc x) y])
        point (point-in graph new-x new-y)
        ky [sym point]
        new-cart (case point
                   (\| \-)
                   cart
                   \+
                   {:n-turns (inc n-turns)
                    :sym (rotate sym n-turns)}
                   {:n-turns n-turns
                    :sym (case ky
                           [\> \\] \v
                           [\> \/] \^
                           [\< \\] \^
                           [\< \/] \v
                           [\^ \\] \<
                           [\^ \/] \>
                           [\v \\] \>
                           [\v \/] \<)})]
    [[new-x new-y] new-cart]))

(defn tick [graph carts]
  (->> carts
       (map (partial transform-cart graph))
       (into (sorted-map))))

(defn print-graph [graph carts]
  (doseq [[y line] (map-indexed vector graph)]
    (println (apply str (for [[x point] (map-indexed vector line)]
                          (or (some-> (carts [x y]) :sym)
                              (cart->line point point)))))))

(defn solution-1 []
  (let [graph (read-sample)
        generations (->> (iterate (partial tick graph) (find-carts graph))
                         (take 5))]
    (doseq [generation generations]
      (print-graph graph generation))))
