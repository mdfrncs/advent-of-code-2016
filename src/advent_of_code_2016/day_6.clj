(ns advent-of-code-2016.day-6
  (:require [clojure.string :as str]))

(defn solve
  [input compare-fn]
  (let [lines (map str/trim (str/split-lines input))
        columns (count (first lines))]
    (-> (sequence
          (comp
            (map (fn [i] (map #(get % i) lines)))
            (map frequencies)
            (map #(sort compare-fn %))
            (map first)
            (map first))
          (range columns))
        str/join)))

(defn solve-pt1
  [input]
  (solve input (fn [[_ i] [_ j]] (compare j i))))

(defn solve-pt2
  [input]
  (solve input (fn [[_ i] [_ j]] (compare i j))))


(def input (slurp "resources/day_6.txt"))