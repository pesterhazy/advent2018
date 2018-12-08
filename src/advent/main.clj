(ns advent.main)

(defn -main
  []
  (let [next-id (->> (file-seq (clojure.java.io/file "src"))
                     (map #(.getName %))
                     (keep #(re-matches #"puzzle(.*)\.clj" %))
                     (map second)
                     (map #(Long/parseLong %))
                     (apply max)
                     inc
                     (format "%02d"))
        fname (str "src/advent/puzzle" next-id ".clj")
        contents (format "(ns advent.puzzle%s)\n" next-id)]
    (spit fname contents)
    (println "=>" fname)))
