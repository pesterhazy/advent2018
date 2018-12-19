(ns advent.puzzle19
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-sample
  []
  (with-open [f (-> "19/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "19/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn process-input
  [lines]
  (let [[header & body] lines]
    {:header (-> (str/split header #"\s")
                 second
                 Long/parseLong)
     :body (->> body
                (mapv (fn [line]
                        (-> (zipmap [:opcode :a :b :c] (str/split line #"\s"))
                            (update :opcode keyword)
                            (update :a #(Long/parseLong %))
                            (update :b #(Long/parseLong %))
                            (update :c #(Long/parseLong %))))))}))

(def i-data (process-input (read-input)))

(def opcodes
  [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr
   :eqir :eqri :eqrr])

(defn gt [a b] (if (> a b) 1 0))

(defn eq [a b] (if (= a b) 1 0))

(defn apply-op
  [regs {:keys [opcode a b c]}]
  (case opcode
    :addr (assoc regs c (+ (regs a) (regs b)))
    :addi (assoc regs c (+ (regs a) b))
    :mulr (assoc regs c (* (regs a) (regs b)))
    :muli (assoc regs c (* (regs a) b))
    :banr (assoc regs c (bit-and (regs a) (regs b)))
    :bani (assoc regs c (bit-and (regs a) b))
    :borr (assoc regs c (bit-or (regs a) (regs b)))
    :bori (assoc regs c (bit-or (regs a) b))
    :setr (assoc regs c (regs a))
    :seti (assoc regs c a)
    :gtir (assoc regs c (gt a (regs b)))
    :gtri (assoc regs c (gt (regs a) b))
    :gtrr (assoc regs c (gt (regs a) (regs b)))
    :eqir (assoc regs c (eq a (regs b)))
    :eqri (assoc regs c (eq (regs a) b))
    :eqrr (assoc regs c (eq (regs a) (regs b)))))

(defn again []
  (let [{:keys [header body]} (process-input (read-sample))]
    (loop [regs [0 0 0 0 0 0]]
      (let [ip (get regs header)]
        (if (<= 0 ip (dec (count body)))
          (let [op (nth body ip)
                _ (prn op)
                new-regs (-> regs
                             (apply-op op))]
            (recur (update new-regs header inc)))
          (nth regs header))))))

;; REPL stuff; ignore.

(defonce bq (java.util.concurrent.LinkedBlockingQueue.))

(defn wait
  "Wait on blocking queue for invocations. When new
  value becomes available, remove and re-run fun.

  User can end the loop by pressing return.

  Useful for calling a test function in a terminal REPL whenever
  the namespace is re-evaluated from a different thread, such
  as an nREPL connection. e.g.:

  (wait solution-2)"
  [fun]
  (.clear bq)
  (prn (fun))
  (loop []
    (if (.poll bq)
      (do
        (println (apply str (repeat 60 "*")))
        (prn (fun))
        (recur))
      (let [n (.available System/in)]
        (if (> n 0)
          (read-line)
          (do
            (Thread/sleep 100)
            (recur))))))
  nil)

;; When ns gets reloaded via nREPL, trigger new call
;; of function passed to wait

(.add bq true)
