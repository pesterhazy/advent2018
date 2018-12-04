(ns advent.puzzle04)

(def regex #"^\[(....-..-..) (..):(..)\].*$")

(defn parse
  [line]
  (let [m (->> (re-matches regex line)
               rest
               ;; (map #(Long/parseLong %))
               (zipmap [:date :h :m]))]
    (assoc m :time (+ (* (Long/parseLong (:h m)) 60)
                      (Long/parseLong (:m m))))))

(defn read-input
  []
  (with-open [f (-> "4/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))
