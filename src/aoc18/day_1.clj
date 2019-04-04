(ns aoc18.day-1
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def input (->> "input1.txt" io/resource io/reader line-seq))

(def data (map read-string input))

(defn part-1 []
  (reduce + data))

(defn first-duplicate [xs]
  (let [result (reduce (fn [seen x]
                         (if (seen x)
                           (reduced x)
                           (conj seen x)))
                       #{} xs)]
    (if (set? result)
      nil
      result)))

(defn part-2 []
  (let [freqs (reductions + (cycle data))]
    (first-duplicate freqs)))
