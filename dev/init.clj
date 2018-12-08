(in-ns 'user)

(require '[nrepl.server])
(require 'rebel-readline.main)
(let [port 48888]
  (def server (nrepl.server/start-server :port port))
  (println "nrepl-port:" port))

(def initial-ns (symbol (str "advent." (->> (file-seq (clojure.java.io/file "src")) (map #(.getName %)) (keep #(re-matches #"(puzzle.*)\.clj" %)) (map second) sort last))))

(require initial-ns)
(in-ns initial-ns)

(rebel-readline.main/-main)

;; done

(shutdown-agents)
(nrepl.server/stop-server user/server)
