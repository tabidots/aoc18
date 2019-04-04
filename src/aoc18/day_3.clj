(ns aoc18.day-3
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def input-lines (->> "input3.txt" io/resource io/reader line-seq))

(defn read-claim [s]
  (->> (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s))
       (map read-string)
       (zipmap [:id :left :top :width :height])))

(def claims (map read-claim input-lines))

(defn in-claim? [[x y] claim]
  (and (<= (:left claim)
           x
           (+ (:left claim) (:width claim) -1))
       (<= (:top claim)
           y
           (+ (:top claim) (:height claim) -1))))

(defn part-1 []
  ;; Brute force. Running time > 6 min
  (->> (for [x (range 1 1001)
             y (range 1 1001)]
         (fnext (filter (partial in-claim? [x y]) claims)))
       (remove nil?)
       count))

(defn overlap? [claim1 claim2]
  (when-not (= claim1 claim2)
    (->> (for [x (range (:left claim1)
                        (+ (:left claim1) (:width claim1)))
               y (range (:top claim1)
                        (+ (:top claim1) (:height claim1)))]
           [x y])
         (some #(in-claim? % claim2)))))

(defn unique-claim? [claim]
  (when (not-any? (partial overlap? claim) claims)
    (:id claim)))

(defn part-2 []
  ;; Also brute force. Running time > 37s
  (some unique-claim? claims))
