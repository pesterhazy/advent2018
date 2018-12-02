(require '[nrepl.server :refer [start-server]])
(let [port 48888]
  (start-server :port port)
  (println "nrepl-port:" port))
