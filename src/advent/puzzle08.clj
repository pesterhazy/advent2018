(ns advent.puzzle08
  (:require [clojure.string :as str]))

(defn read-sample
  []
  (-> "8/sample.txt"
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
