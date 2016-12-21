(ns advent-of-code-2016.day-20
  (:require [clojure.string :as str]
            [advent-of-code-2016.util :as util]))

(defn to-filter
  [line]
  (let [split (str/split line #"-")]
    [(util/to-long (first split)) (util/to-long (last split))]))

(defn ip-filters
  [input]
  (let [lines (map str/trim (str/split-lines input))
        ip-filters (map to-filter lines)]
    (into [] (sort (fn [[s1 _] [s2 _]] (compare s1 s2)) ip-filters))))

(defn solve-pt1
  [input]
  (let [ip-filters (ip-filters input)]
    (reduce (fn [smallest [start end]]
              (if (< smallest start)
                (reduced smallest)
                (max smallest (inc end))))
            0
            ip-filters)))

(defn solve-pt2
  [input]
  (let [ip-filters (ip-filters input)]
    (last (reduce (fn [[smallest allowed] [start end]]
                    [(max smallest (inc end))
                     (+ allowed (if (< smallest start) (- start smallest) 0))])
                  [0 0]
                  ip-filters))))

(def input (slurp "resources/day_20.txt"))