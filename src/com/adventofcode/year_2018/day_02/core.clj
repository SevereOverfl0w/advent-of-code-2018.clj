(ns com.adventofcode.year-2018.day-02.core
  (:require
    [clojure.test :as t]))

(set! *warn-on-reflection* true)

(defn relevant-frequencies
  [s]
  (distinct (keep (fn [[k v]] (when (> v 1) v)) (frequencies s))))

(defn checksum
  [ss]
  (apply * (vals (frequencies (mapcat relevant-frequencies ss)))))

;;

(t/deftest examples
  (t/are [input output] (= (sort output)
                           (sort (relevant-frequencies input)))
         "abcdef" []
         "bababc" [2 3]
         "abbcde" [2]
         "abcccd" [3]
         "aabcdd" [2]
         "abcdee" [2]
         "ababab" [3])
  (t/is (= (checksum ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]) 12)))

(comment
  (def input (clojure.string/split-lines (slurp (clojure.java.io/resource "com/adventofcode/year_2018/day_02/input"))))
  (checksum input)
  )

;; part 2

(def part-2-example ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn similarity
  [s1 s2]
  (count (filter identity (map not= s1 s2))))

(defn remove-different
  [s1 s2]
  (apply str (map first (remove #(apply not= %) (map vector s1 s2)))))

(defn remove-different2
  ^java.lang.StringBuilder [^String s1 ^String s2]
  (let [sb (java.lang.StringBuilder.)]
    (dotimes [idx (.length ^String s1)]
      (let [c1 (.charAt s1 idx)]
        (when (= c1 (.charAt s2 idx))
          (.append sb c1))))
    sb)) 

(defn common-letters
  [ss]
  (let [[_ s1 s2]
        (apply min-key first
               (for [s1 ss
                     s2 ss
                     :when (not= s1 s2)]
                 [(similarity s1 s2) s1 s2]))]
    (remove-different s1 s2)))

(defn common-letters2
  [ss]
  (str
    (apply max-key
           (fn [sb]
             (.length ^java.lang.StringBuilder sb))
           (for [s1 ss
                 s2 ss
                 :when (not= s1 s2)]
             (remove-different2 s1 s2)))))

(t/deftest part-2
  (t/are [s1 s2 output] (= output (similarity s1 s2))
         "abcde" "axcye" 2
         "fghij" "fguij" 1)
  (t/is
    (= "fgij" (str (remove-different2 "fghij" "fguij"))))
  
  (t/is
    (= "fgij" (common-letters2 part-2-example))))

(comment
  (require '[clj-async-profiler.core :as prof])
  (do
    (prof/start {})
    (common-letters2 input)
    (prof/stop {}))
  (require '[criterium.core :refer [quick-bench bench]])
  (quick-bench (common-letters input))
  (bench (common-letters2 input)))
