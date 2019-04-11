(ns aoc18.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [clojure.pprint :refer [pprint]]))

(def day-6-input
  (->> "input6.txt" io/resource io/reader line-seq))

(def coords
  (reduce (fn [res coord-str]
            (let [[x y] (map #(Integer/parseInt %) (split coord-str #", "))]
              (assoc res (count res) {:x x :y y})))
          {} day-6-input))

(def bounds
  "Get the bounds of the set of coordinates, as it's not necessary to consider
  any points lying beyond them."
  (let [xs (map (fn [[id xy]] (:x xy)) coords)
        ys (map (fn [[id xy]] (:y xy)) coords)]
    {:min-x (apply min xs) :min-y (apply min ys)
     :max-x (apply max xs) :max-y (apply max ys)}))

(defn on-perimeter?
  "Check if a given point lies on the perimeter of the search space, since the
  coordinates with infinite areas are the ones that those points are closest to."
  [x y]
  (or (= x (:min-x bounds)) (= x (:max-x bounds))
      (= y (:min-y bounds)) (= y (:max-y bounds))))

(defn manhattan-distance
  [x1 y1 {x2 :x y2 :y}]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn closest-coord
  "A sorted-map with Manhattan distances from a point (x, y) as keys
  in ascending order, where each value is either the ID of the unique coordinate
  at that distance or nil if there is more than one coordinate at that distance."
  [x y]
  (->> (reduce-kv (fn [res id coord]
                    (let [dist (manhattan-distance x y coord)]
                      (if (get res dist)
                        (assoc res dist nil) ;; equidistant to 2+ pts
                        (assoc res dist id))))
                  (sorted-map) coords)
       first
       last))

(defn part-1
  "What is the size of the largest non-infinite area around any of the given
  coordinates, where area is determined by the number of points in the grid
  whose Manhattan distance to that coordinate is shorter than the Manhattan
  distance to any other coordinate. This is basically a Voronoi diagram, where
  we are looking for the largest 'landlocked' piece, so to speak."
  []
  (let [closest-coords (->> (for [x (range (:min-x bounds) (inc (:max-x bounds)))
                                  y (range (:min-y bounds) (inc (:max-y bounds)))]
                              (if (on-perimeter? x y)
                                {:infinite (closest-coord x y)}
                                {:finite   (closest-coord x y)}))
                            (apply merge-with conj {:infinite #{} :finite []}))]
    (->> (:finite closest-coords)
         (remove (:infinite closest-coords))
         (frequencies)
         (apply max-key val)
         (peek))))

(defn part-2
  "How many points have a sum distance to all given coordinates of less than 10000?"
  []
  (->> (for [x (range (:min-x bounds) (inc (:max-x bounds)))
             y (range (:min-y bounds) (inc (:max-y bounds)))]
         (reduce (fn [res coord]
                   (+ res (manhattan-distance x y (val coord))))
                 0 coords))
       (filter #(< % 10000))
       (count)))
