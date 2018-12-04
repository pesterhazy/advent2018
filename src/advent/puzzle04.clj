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
       sort
       (map parse)
       fill-in
       (filter #(= :end (:event %)))
       (map
        (fn [m]
          {:id (:id m), :date (:date m), :start (:start m), :end (:min m)}))))

(defn solution-1-and-2
  [es]
  (let [id->es (group-by :id es)
        ;; s1
        id->durations (->> id->es
                           (map (fn [[id es]]
                                  [id
                                   (->> es
                                        (map (fn [e] (- (:end e) (:start e))))
                                        (apply +))]))
                           (into {}))
        sleepiest-id (->> id->durations
                          (sort-by (comp - second))
                          ffirst)
        es (id->es sleepiest-id)
        sleepiest-min (->> es
                           (map (juxt :start :end))
                           (mapcat (partial apply range))
                           frequencies
                           (sort-by (comp - second))
                           ffirst)
        ;; s2
        id+mins (->> id->es
                     (mapcat (fn [[id es]]
                               (->> es
                                    (map (juxt :start :end))
                                    (mapcat (partial apply range))
                                    (map vector (repeat id))))))
        sleepiest-id+min (->> id+mins
                              frequencies
                              (sort-by (comp - second))
                              ffirst)]
    {:solution-1 (* sleepiest-min sleepiest-id),
     :solution-2 (apply * sleepiest-id+min)}))

(defn read-sample
  []
  (with-open [f (-> "4/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "4/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))
