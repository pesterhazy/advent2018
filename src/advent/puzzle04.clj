(ns advent.puzzle04)

(def regex #"^\[(....-..-..) (..):(..)\] (.*)$")

(defn parse
  [line]
  (let [m (->> (re-matches regex line)
               rest
               ;; (map #(Long/parseLong %))
               (zipmap [:date :h :m :text]))
        m* (assoc m
                  :time (+ (* (Long/parseLong (:h m)) 60) (Long/parseLong (:m m))))]
    (if-let [g (re-matches #"Guard #(\d+) begins shift" (:text m))]
      (assoc m*
             :event :begin
             :id (Long/parseLong (second g)))
      (assoc m* :event ({"wakes up" :start, "falls asleep" :end} (:text m))))))

(defn fill-in
  [xs]
  (reduce (fn [[acc-xs acc-id] x]
            (let [id (or (:id x) acc-id)] [(conj acc-xs (assoc x :id id)) id]))
          [[] nil]
          xs))

(defn read-input
  []
  (with-open [f (-> "4/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))
