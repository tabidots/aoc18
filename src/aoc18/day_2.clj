(ns aoc18.day-2
  (:require [clojure.java.io :as io]
            [clojure.set :refer [difference intersection]]
            [clojure.pprint :refer [pprint]]))

(def input (->> "input2.txt" io/resource io/reader line-seq))

(defn has-two? [coll]
  (let [distinct-freqs (set (vals (frequencies coll)))]
    (boolean (distinct-freqs 2))))

(defn has-three? [coll]
  (let [distinct-freqs (set (vals (frequencies coll)))]
    (boolean (distinct-freqs 3))))

(defn part-1 []
  (let [has-twos   (filter has-two? input)
        has-threes (filter has-three? input)]
    (* (count has-twos)
       (count has-threes))))

(defn indexed-chars [s]
  (set (map-indexed #(vector %1 (identity %2)) s)))

(defn diff-chars [s1 s2]
  (difference (indexed-chars s1) (indexed-chars s2)))

(defn off-by-one-counterpart [s]
  (filter #(= (count (diff-chars s %)) 1)
          input))

(defn target-pair []
  (->> input
       (map off-by-one-counterpart)
       (filter seq)     ; filter non-nil values
       (apply concat))) ; join into a single seq

(defn common-chars [s1 s2]
  (intersection (indexed-chars s1) (indexed-chars s2)))

(defn part-2 []
  (->> (apply common-chars (target-pair))
       vec
       sort
       (map last)
       (apply str)))
