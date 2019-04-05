(ns aoc18.day-5
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [join]]))

(def day-5-input
  (->> "input5.txt" io/resource io/reader line-seq first))

(defn reactive?
  "Two units of a polymer are reactive if they are the same letter
  in different cases."
  [a b]
  (= 32 (Math/abs (- (int a) (int b)))))

(defn trigger [input-string]
  ;; Constructing the reacted sequence backwards is much faster
  ;; than using vectors to construct it in left-to-right order.
  (->> (reduce (fn [a b]
                 (if-let [tail (first a)]
                   (if (reactive? tail b)
                     (rest a)
                     (cons b a))
                   (cons b a)))
               '() (seq input-string))
       reverse))

(defn part-1
  "How many units remain after fully reacting the polymer?"
  []
  (count (trigger day-5-input)))

(defn part-2
  "What is the length of the shortest polymer you can produce by
  removing all units of exactly one type and fully reacting the result?"
  []
  (->> (range (int \A) (inc (int \Z)))         ;; range from \A to \Z
       (map #(set [(char %) (char (+ 32 %))])) ;; pairs #{\A \a} ... #{\B \b}
       (map #(count (trigger (remove % day-5-input))))
       (apply min)))
