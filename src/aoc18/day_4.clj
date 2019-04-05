(ns aoc18.day-4
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [join]]))

(def days-in-months
  "One-indexed number of days in months. Necessary for incrementing dates."
  [0 31 28 31 30 31 30 31 31 30 31 30 31])

;; Assume this data structure: {date {:guard id :times [sleep wake sleep wake]}}
(defn sleeping-minutes
  "Given a coll of numbers representing minutes (assuming they are distinct
  and paired), sort them ascending, and treat them like [sleep-wake sleep-wake],
  then transform each sleep-wake into a range in order to expand the exact
  minutes of each sleeping session."
  [coll]
  (->> (sort coll)
       (partition 2 2)
       (mapcat #(range (first %) (last %)))))

(defn starts-before-midnight?
  "Determines if a shift starts before midnight so that it can be treated as
  belonging to the next calendar date."
  [s]
  (re-find #"23:" s))

(defn next-day [[m-string d-string]]
  "Given two zero-padded strings representing month and date, returns
  two zero-padded strings representing the following month and date
  (assuming the year is not a leap year)."
  (let [m        (Integer/parseInt m-string)
        inc-date (-> (Integer/parseInt d-string)
                     inc)
        date     (if (> inc-date (get days-in-months m)) 1 inc-date)
        month    (if (= 1 date) (inc m) m)]
    (str (when (< month 10) "0")
         month
         (when (< date 10) "0")
         date)))

(defn add-log-entry
  "Adds an entry to entries. Each single entry does not completely describe
  a single shift, so when used as a reducer function, this will accumulate
  data in a hash-map with date strings as keys (e.g., August 11 -> '0811'),
  and the shift info as the corresponding val, in the following way:
  {:guard id :times [1 2 3 4 5 6]} where the times will be processed using
  the sleeping-times function above."
  [entries entry]
  (let [base-date  (rest (re-find #"\d{4}-(\d{2})-(\d{2})" entry))
        date       (if (starts-before-midnight? entry)
                     (next-day base-date)
                     (join base-date))
        time       (first (rest (re-find #":(\d{2})" entry)))
        guard      (first (rest (re-find #"#(\d+)" entry)))]
    (if guard
      (assoc entries date {:guard (Integer/parseInt guard)})
      (update-in entries [date :times] conj (Integer/parseInt time)))))

(def sample-entries
  (->> "input4_sample.txt" io/resource io/reader line-seq))

(def day-4-input
  (->> "input4.txt" io/resource io/reader line-seq))

(defn parse-log
  "Parses a log and re-maps the input to a new map whose keys are guard IDs
  and vals are a collection of numbers representing sleeping minutes (across
  all of that guard's shifts)."
  [input]
  (->> (sort input)
       ;; ↓ {date {:guard id :times [sleep wake sleep wake]}}
       (reduce add-log-entry {})
       ;; ↓ {guard [1 2 3 9 10 11], guard [4 5 6 7 8]}
       (reduce-kv (fn [m date {ts :times g :guard}]
                    (let [minutes (sleeping-minutes ts)]
                      (update m g into minutes)))
                  {})))

(defn sleepiest-guard-and-minute
  "Find the guard that has the most minutes asleep.
  What minute does that guard spend asleep the most?"
  [input]
  (let [log              (parse-log input)
        guards           (keys log)
        sleepiest-guard  (apply max-key #(count (get log %)) guards)
        minute-freqs     (frequencies (get log sleepiest-guard))
        minutes          (keys minute-freqs)
        sleepiest-minute (apply max-key #(get minute-freqs %) minutes)]
    (* sleepiest-guard sleepiest-minute)))

(defn part-1 []
  (sleepiest-guard-and-minute day-4-input))

(defn highest-frequency-sleepy-minute
  "Of all guards, which guard is most frequently asleep on the same minute?
  What is the ID of the guard you chose multiplied by the minute you chose?"
  [input]
  (->> (parse-log input)
       ;; ↓ re-map the data to a new map whose keys are the non-unique
       ;; frequencies of the most-slept minute of each guard.
       (reduce-kv (fn [m g minutes]
                    (if (empty? minutes) m ;; sanity check because the data is dirty
                      (let [[minute freq] (apply max-key val (frequencies minutes))]
                        ;; Avoid headaches later by just multiplying ID × minute here
                        (conj m {freq (* g minute)}))))
                  {})
       (apply max-key key) ;; get the pair [max-freq id-minute-product]
       last))              ;; just the id-minute-product, please

(defn part-2 []
  (highest-frequency-sleepy-minute day-4-input))
