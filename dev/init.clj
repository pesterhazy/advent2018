(require '[nrepl.server])
(let [port 48888]
  (def server (nrepl.server/start-server :port port))
  (println "nrepl-port:" port))

(require 'rebel-readline.main)
(rebel-readline.main/-main)

(shutdown-agents)
(nrepl.server/stop-server server)
