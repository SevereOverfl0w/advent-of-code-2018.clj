(ns com.adventofcode.year-2018.day-01.core
  (:require
    [clojure.test :as t]
    [clojure.string :as string]
    [clojure.java.io :as io]))

(defn input-actions
  [input]
  (map #(Integer/parseInt %) (string/split-lines input)))

(defn calc-part1
  [input]
  (reduce + (input-actions input)))

(t/deftest examples-part1
  (t/are [input result] (= result (calc-part1 input))
         "+1\n+1\n+1\n" 3
         "+1\n+1\n-2\n" 0
         "-1\n-2\n-3\n" -6))

(comment
  (calc-part1 (slurp (io/resource "com/adventofcode/year_2018/day_01/input"))))

(defn device-frequencies
  [actions]
  ;; Providing a val is important to get the initial 0 in the input
  (reductions + 0 actions))

(defn calc-part2
  [input]
  (reduce
    (fn [seen input]
      (or (some-> (get seen input) reduced)
          (conj! seen input)))
    (transient #{})
    (device-frequencies (cycle (input-actions input)))))

(comment
  (calc-part2 (slurp (io/resource "com/adventofcode/year_2018/day_01/input"))))

(t/deftest examples-part2
  (t/are [input result] (= result (calc-part2 input))
         "+1\n-1\n" 0
         "+3\n+3\n+4\n-2\n-4\n" 10
         "-6\n+3\n+8\n+5\n-6\n" 5
         "+7\n+7\n-2\n-7\n-4\n" 14))
