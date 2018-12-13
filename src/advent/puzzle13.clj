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
         [[y x] {:sym point
                 :n-turns 0}])
       (map-indexed (fn [idx [xy m]]
                      [xy (assoc m :id idx)]))
       (into (sorted-map))))

(defn rotate [sym nturns]
  (case (mod nturns 3)
    0 (nth directions (mod (dec (.indexOf directions sym)) 4))
    1 sym
    2 (nth directions (mod (inc (.indexOf directions sym)) 4))))

(defn transform-cart [graph [[y x] {:keys [n-turns sym] :as cart}]]
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
    [[new-y new-x] new-cart]))

(defn tick [graph remove? carts]
  (let [[collisions new-carts]
        (->> carts
             (reduce (fn [[acc-collisions acc-carts] cart]
                       (let [[yx m :as new-cart] (transform-cart graph cart)
                             target (or (acc-carts yx)
                                        (carts yx))]
                         [(cond-> acc-collisions
                            target
                            (conj (:id m)
                                  (:id target)))
                          (conj acc-carts new-cart)]))
                     [#{} (sorted-map)]))]
    (when (seq collisions)
      (prn {:ids (set (map :id (vals new-carts)))
            :collisions collisions}))
    (if remove?
      (->> new-carts
           (remove (fn [[_ m]]
                     (contains? collisions (:id m))))
           (into (sorted-map)))
      (->> new-carts
           (map (fn [[yx m]]
                  [yx (cond-> m
                        (contains? collisions (:id m))
                        (assoc :collision yx))]))
           (into (sorted-map))))))

(defn print-graph [graph carts]
  (doseq [[y line] (map-indexed vector graph)]
    (println (apply str (for [[x point] (map-indexed vector line)]
                          (or (some-> (carts [y x]) :sym)
                              (cart->line point point)))))))

(defn solution-1 []
  (let [graph (read-input)
        generations (iterate (partial tick graph false) (find-carts graph))]
    (->> (some (fn [generation]
                 (some :collision (vals generation)))
               generations)
         reverse
         (str/join ","))))

(defn solution-2 []
  (spit "log.txt" "")
  (let [graph (read-input)
        generations (iterate (partial tick graph true) (find-carts graph))]
    (some (fn [[prev-generation generation]]
            (when (= 1 (count generation))
              ;; print out previous generation for debugging
              #_(prn prev-generation)
              #_(prn generation)
              (str/join "," (->> generation first first reverse))))
          (partition 2 generations))))
