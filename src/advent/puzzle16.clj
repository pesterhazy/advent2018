(ns advent.puzzle16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-sample
  []
  (with-open [f (-> "16/sample.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(defn read-input
  []
  (with-open [f (-> "16/input.txt"
                    clojure.java.io/reader)]
    (vec (line-seq f))))

(def regex
  #"(?x)
Before:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]\n
(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\n
After:\s+\[(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\]\s*")

(defn match
  [lines]
  (let [matches (re-matches regex (str/join "\n" lines))]
    (assert matches)
    (-> (->> (rest matches)
             (partition 4)
             (map (partial mapv #(Long/parseLong %)))
             (zipmap [:before :op :after]))
        (update :op (partial zipmap [:opcode :a :b :c])))))

(defn process-input
  [lines]
  (let [[part1 part2] (->> (read-input)
                           (partition-all 2 1)
                           (split-with (fn [[a b]]
                                         (or (not= "" a) (not= "" b))))
                           (mapv (partial map first)))]
    {:patterns (->> part1
                    (partition-all 4)
                    (map match))
     :instructions (->> part2
                        (drop-while str/blank?)
                        (mapv (fn [line]
                                (->> (str/split line #"\s+")
                                     (mapv #(Long/parseLong %))
                                     (zipmap [:opcode :a :b :c])))))}))

(def i-data (process-input (read-input)))

(def opcodes
  [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr
   :eqir :eqri :eqrr])

(defn gt [a b] (if (> a b) 1 0))

(defn eq [a b] (if (= a b) 1 0))

(defn apply-op
  [regs {:keys [opcode a b c] :as op}]
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

(defn try-pattern-one
  [pattern substitute]
  (= (apply-op (:before pattern) (assoc (:op pattern) :opcode substitute))
     (:after pattern)))

(defn try-pattern
  [pattern]
  (->> opcodes
       (keep (fn [opcode] (when (try-pattern-one pattern opcode) opcode)))
       set))

(defn solution-1 []
  (let [patterns (:patterns i-data)
        three-or-more (->> patterns
                           (map (comp count try-pattern))
                           (filter #(>= % 3))
                           count)]
    {:n-patterns (count patterns)
     :three-or-more three-or-more}))

(defn find-mapping [patterns]
  (loop [mapping {}
         opcode->ops (->> patterns
                          (reduce (fn [acc-map pattern]
                                    (let [candidates (try-pattern pattern)]
                                      (update acc-map (-> pattern :op :opcode)
                                              (fn [old-candidates]
                                                (if old-candidates
                                                  (set/intersection old-candidates candidates)
                                                  candidates)))))
                                  {}))]
    (let [new-mapping (->> opcode->ops
                           (keep (fn [[opcode ops]]
                                   (when (= 1 (count ops))
                                     [opcode (first ops)])))
                           (into mapping))
          new-opcode->ops (let [done (->> mapping
                                          vals
                                          set)]
                            (->> opcode->ops
                                 (map (fn [[opcode ops]]
                                        [opcode (set/difference ops done)]))
                                 (filter (fn [[_ ops]]
                                           (seq ops)))))]
      (if (seq new-opcode->ops)
        (recur new-mapping
               new-opcode->ops)
        new-mapping))))

(defn solution-2 []
  (let [opcode->op (find-mapping (:patterns i-data))]
    (->> (:instructions i-data)
         (map #(update % :opcode opcode->op))
         (reduce apply-op [0 0 0 0])
         first)))

;; REPL stuff; ignore.

(defonce bq (java.util.concurrent.LinkedBlockingQueue.))

(defn wait
  "Wait on blocking queue for invocations. When new
  value becomes available, remove and re-run fun.

  User can end the loop by pressing return.

  Useful for calling a test function in a terminal REPL whenever
  the namespace is re-evaluated from a different thread, such
  as an nREPL connection"
  [fun]
  (.clear bq)
  (prn (fun))
  (loop []
    (if (.poll bq)
      (do
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
