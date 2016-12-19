(ns advent-of-code-2016.day_18
  (:require [clojure.string :as str]))


(defn room
  [idx row]
  (or (get row idx) \.))

(defn gen-room
  [row idx]
  (let [input (str/join [(room (dec idx) row)
                         (room idx row)
                         (room (inc idx) row)])]
    (case input
      "^^." \^
      ".^^" \^
      "^.." \^
      "..^" \^
      \.)))

(def next-row
  (memoize (fn [input]
             (str/join (map (partial gen-room input)
                            (range (count input)))))))

(def safe-in-row
  (memoize (fn [line]
             (count (filter #(= % \.) line)))))

(defn safe-rooms
  [input num]
  (loop [idx 1
         safe (safe-in-row input)
         last input]
    (when (= idx num)
      safe
      (let [next-row (next-row last)]
        (recur (inc idx)
               (+ safe (safe-in-row next-row))
               next-row)))))

(defn solve-pt1
  [input]
  (safe-rooms input 40))

(defn solve-pt2
  [input]
  (safe-rooms input 400000))


(def input "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^.......")
