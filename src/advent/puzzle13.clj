(ns advent.puzzle13
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn read-sample-2
  []
  (with-open [f (-> "13/sample2.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def cart->line {\^ \|
                 \> \-
                 \v \-
                 \< \|})

(def directions [\^ \> \v \<])

(defn point-in [graph x y]
  (let [point (nth (nth graph y) x)
        point* (or (cart->line point) point)]
    (assert (#{\- \| \+ \\ \/} point*)
            (str "Invariant failed: " (pr-str point*)))
    point*))

(defn find-carts [graph]
  (->> (for [[y line] (map-indexed vector graph),
             [x point] (map-indexed vector line)
             :when (cart->line point)]
         {:x x
          :y y
          :sym point
          :n-turns 0})
       (map-indexed (fn [idx m]
                      [idx (assoc m :id idx)]))
       (into {})))

(defn rotate [sym nturns]
  (case (mod nturns 3)
    0 (nth directions (mod (dec (.indexOf directions sym)) 4))
    1 sym
    2 (nth directions (mod (inc (.indexOf directions sym)) 4))))

(defn transform-cart [graph {:keys [x y n-turns sym] :as cart}]
  (let [[new-y new-x]
        (case sym
          \^ [(dec y) x]
          \v [(inc y) x]
          \< [y (dec x)]
          \> [y (inc x)])
        point (point-in graph new-x new-y)
        new-cart (case point
                   (\| \-)
                   cart
                   \+
                   (assoc cart
                          :n-turns (inc n-turns)
                          :sym (rotate sym n-turns))
                   (\\ \/)
                   (assoc cart
                          :sym (case [sym point]
                                 [\> \\] \v
                                 [\> \/] \^
                                 [\< \\] \^
                                 [\< \/] \v
                                 [\^ \\] \<
                                 [\^ \/] \>
                                 [\v \\] \>
                                 [\v \/] \<)))]
    (assoc new-cart
           :x new-x
           :y new-y)))

(defn tick [graph remove? carts]
  (let [[collisions new-carts]
        (->> carts
             (reduce (fn [[acc-collisions acc-carts] [cart-id cart]]
                       (let [new-cart
                             (transform-cart graph cart),

                             collision-id
                             (->> acc-carts
                                  (sort-by (fn [[_ m]]
                                             [(:y m) (:x m)]))
                                  (some (fn [[id m]]
                                          (when (and (= (:x new-cart) (:x m))
                                                     (= (:y new-cart) (:y m)))
                                            id))))]
                         [(cond-> acc-collisions
                            collision-id
                            (conj cart-id
                                  collision-id))
                          (assoc acc-carts cart-id new-cart)]))
                     [#{} carts]))]
    (if remove?
      (->> new-carts
           (remove (fn [[id _]]
                     (contains? collisions id)))
           (into {}))
      (->> new-carts
           (map (fn [[id cart]]
                  [id (cond-> cart
                        (contains? collisions id)
                        (assoc :collision [(:x cart) (:y cart)]))]))
           (into {})))))

(defn print-graph [graph carts]
  (let [xy->cart (-> carts vals (set/index [:x :y]))]
    (doseq [[y line] (map-indexed vector graph)]
      (println (apply str (for [[x point] (map-indexed vector line)]
                            (or (some-> (xy->cart {:x x :y y})
                                        first
                                        :sym)
                                (cart->line point point))))))
    (println)))

(defn solution-1 []
  (let [graph (read-sample)
        generations (iterate (partial tick graph false) (find-carts graph))]
    (->> (some (fn [generation]
                 (print-graph graph generation)
                 (some :collision (vals generation)))
               generations)
         (str/join ","))))

(defn solution-2 []
  (let [graph (read-input)
        generations (iterate (partial tick graph true) (find-carts graph))
        final-generation (some (fn [generation]
                                 (when (= 1 (count generation))
                                   generation))
                               generations)]
    (str  (-> final-generation first second :x)
          ","
          (-> final-generation first second :y))))
