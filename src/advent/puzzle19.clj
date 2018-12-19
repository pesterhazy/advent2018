(ns advent.puzzle19
  (:require [clojure.string :as str]))

#_(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

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

(defn gt [^long a ^long b] (if (> a b) 1 0))

(defn eq [^long a ^long b] (if (= a b) 1 0))

(defn apply-op
  [^longs regs {:keys [opcode ^long a ^long b ^long c]}]
  (case opcode
    :addr (aset-long regs c (+ ^long (aget regs a) ^long (aget regs b)))
    :addi (aset-long regs c (+ ^long (aget regs a) b))
    :mulr (aset-long regs c (* ^long (aget regs a) ^long (aget regs b)))
    :muli (aset-long regs c (* ^long (aget regs a) b))
    :banr (aset-long regs c (bit-and ^long (aget regs a) ^long (aget regs b)))
    :bani (aset-long regs c (bit-and ^long (aget regs a) b))
    :borr (aset-long regs c (bit-or ^long (aget regs a) ^long (aget regs b)))
    :bori (aset-long regs c (bit-or ^long (regs a) b))
    :setr (aset-long regs c (aget regs a))
    :seti (aset-long regs c a)
    :gtir (aset-long regs c (gt a (aget regs b)))
    :gtri (aset-long regs c (gt (aget regs a) b))
    :gtrr (aset-long regs c (gt (aget regs a) (aget regs b)))
    :eqir (aset-long regs c (eq a (aget regs b)))
    :eqri (aset-long regs c (eq (aget regs a) b))
    :eqrr (aset-long regs c (eq (aget regs a) (aget regs b)))))

(defn execute [initial-regs]
  (let [regs (long-array initial-regs)
        {:keys [^long header body]} (process-input (read-input))]
    (loop [i 0]
      (when (zero? (mod i 10000000))
        (println i))
      (let [ip (aget regs header)]
        (if (<= 0 ip (dec (count body)))
          (let [op (nth body ip)]
            (apply-op regs op)
            (aset-long regs header (inc (aget regs header)))
            (recur (inc i)))
          (nth regs 0))))))

(defn again []
  (time (execute [0 0 0 0 0 0])))


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
  (.clear ^java.util.concurrent.LinkedBlockingQueue bq)
  (prn (fun))
  (loop []
    (if (.poll ^java.util.concurrent.LinkedBlockingQueue bq)
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

(.add ^java.util.concurrent.LinkedBlockingQueue bq true)
