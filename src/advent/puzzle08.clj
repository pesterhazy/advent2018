(ns advent.puzzle08
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (-> "8/sample.txt"
      slurp
      (str/split #"\s")))

(defn read-input
  []
  (-> "8/input.txt"
      slurp
      (str/split #"\s")))

(defn parse [xs]
  (map #(Long/parseLong %) xs))

(defn read-node* [[n-children n-entries & rst]]
  (let [[children remaining-xs]
        (loop [acc []
               xs rst
               i n-children]
          (if (zero? i)
            [acc xs]
            (let [[node remaining-xs] (read-node* xs)]
              (recur (conj acc node) remaining-xs (dec i)))))
        [entries remaining-xs]
        (split-at n-entries remaining-xs)]
    [{:children children
      :entries entries}
     remaining-xs]))

(defn read-node [xs]
  (-> (read-node* xs) first))

(defn solution-1 []
  (->> (read-input)
       parse
       read-node
       (tree-seq map? :children)
       (mapcat :entries)
       (apply +)))

(defn value [{:keys [entries children] :as node}]
  (prn "NODE" node)
  (if (seq children)
    (->> entries
         (map (fn [n]
                (prn (get children n 0))
                (-> (get children n 0) value)))
         (apply +))
    (do
      (prn [:no-children (apply + entries) entries])
      (apply + entries))))
