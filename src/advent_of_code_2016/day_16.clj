(ns advent-of-code-2016.day-16
  (:require [clojure.string :as str]))

(defn dragon-curve
  [a]
  (let [b (str/join (map #(case % \0 \1
                                  \1 \0)
                         (reverse a)))]
    (str a "0" b)))

(defn fill-disk
  [input size]
  (loop [result input]
    (if (>= (count result) size)
      result
      (recur (dragon-curve result)))))

(defn checksum
  [input]
  (str/join (map (fn [[a b]]
                   (if (= a b) 1 0))
                 (partition 2 input))))

(defn odd-checksum
  [input size]
  (loop [cksm (checksum (take size input))]
    (if (odd? (count cksm))
      cksm
      (recur (checksum cksm)))))

(defn solve
  [input size]
  (odd-checksum (fill-disk input size) size))

(defn solve-pt1
  [input]
  (solve input 272))

(defn solve-pt2
  [input]
  (solve input 35651584))

(def input "10111011111001111")