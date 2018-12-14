(ns advent.puzzle14)

(def initial-state {:positions [0 1], :nums [3 7]})

(defn ->recipes
  [a b]
  (let [sum (+ a b)]
    (->> (str sum)
         (map (fn [c] (- (int c) 48))))))

(defn next-state
  [init-state]
  (let [new-nums (apply ->recipes (map (:nums init-state) (:positions init-state)))
        {:keys [nums], :as state*} (update init-state :nums into new-nums)]
    (update state*
            :positions
            (fn [positions]
              (->> positions
                   (mapv (fn [position]
                           (mod (+ position (nums position) 1) (count nums)))))))))
