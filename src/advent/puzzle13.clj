(ns advent.puzzle13
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (with-open [f (-> "13/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "13/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def cart->line {\^ \|
                 \> \-
                 \v \-
                 \< \|})

(def directions [\^ \> \v \<])

(defn point-in [graph x y]
  (let [point (nth (nth graph y) x)]
    (or (cart->line point) point)))

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
       (reduce (fn [acc-carts cart]
                 (let [[xy m :as new-cart] (transform-cart graph cart)]
                   (if (acc-carts xy)
                     (conj acc-carts [xy (assoc m :collision xy)])
                     (conj acc-carts new-cart))))
               (sorted-map))))

(defn print-graph [graph carts]
  (doseq [[y line] (map-indexed vector graph)]
    (println (apply str (for [[x point] (map-indexed vector line)]
                          (or (some-> (carts [x y]) :sym)
                              (cart->line point point)))))))

(defn solution-1 []
  (let [graph (read-input)
        generations (iterate (partial tick graph) (find-carts graph))]
    (->> (some (fn [generation]
                 (some :collision (vals generation)))
               generations)
         (str/join ","))))
