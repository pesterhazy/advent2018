(ns advent.puzzle04)

(def regex #"^\[(....-..-..) ..:(..)\] (.*)$")

(defn parse
  [line]
  (let [m (->> (re-matches regex line)
               rest
               ;; (map #(Long/parseLong %))
               (zipmap [:date :min :text]))
        m* (update m :min #(Long/parseLong %))]
    (if-let [g (re-matches #"Guard #(\d+) begins shift" (:text m))]
      (assoc m*
             :event :set-guard
             :id (Long/parseLong (second g)))
      (assoc m* :event ({"wakes up" :end, "falls asleep" :start} (:text m))))))

(defn fill-in
  [ms]
  (-> (reduce (fn [[acc-ms acc-id acc-start] m]
                (let [id (or (:id m) acc-id)
                      start (if (= :start (:event m)) (:min m) acc-start)]
                  [(conj acc-ms
                         (assoc m
                                :id id
                                :start start)) id start]))
              [[] nil nil]
              ms)
      first))

(defn events
  [lines]
  (->> lines
       (map parse)
       fill-in
       (filter #(= :end (:event %)))
       (map
        (fn [m]
          {:id (:id m), :date (:date m), :start (:start m), :end (:min m)}))))

(defn solution-1
  [es]
  (let [id->es (group-by :id es)
        id+durations (map (fn [[id es]] [id
                                         (->> es
                                              (map (fn [e]
                                                     (- (:end e) (:start e))))
                                              (apply +))])
                          id->es)
        sleepiest-id (->> id+durations
                          (sort-by (comp - second))
                          first
                          first)]
    sleepiest-id))

(defn read-input
  []
  (with-open [f (-> "4/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))
