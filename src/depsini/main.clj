(ns depsini.main)

(defn -main [& args]
  (println "despini")
  (when (= "-m" (first args))
    (let [klass (second args)]
      (require (symbol klass))
      (apply (resolve (symbol klass "-main"))
             (drop 2 args)))))
