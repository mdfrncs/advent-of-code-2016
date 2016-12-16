(ns advent-of-code-2016.day-9
  (:require [advent-of-code-2016.util :as util]))

(defn solve-pt1
  [s]
  (let [matcher (re-matcher #"([A-Z]*)\(?([0-9]*)x?([0-9]*)\)?(.*)" s)
        [_ p n t last] (re-find matcher)
        num (util/to-int n)
        times (util/to-int t)]
    (if times
      (+ (count p)
         (* times num)
         (solve-pt1 (subs last num)))
      (count p))))

(defn solve-pt2
  [s]
  (let [matcher (re-matcher #"([A-Z]*)\(?([0-9]*)x?([0-9]*)\)?(.*)" s)
        [_ p n t last] (re-find matcher)
        num (util/to-int n)
        times (util/to-int t)]
    (if times
      (+ (count p)
         (* times (solve-pt2 (subs last 0 num)))
         (solve-pt2 (subs last num)))
      (count p))))


(def input (slurp "resources/day_9.txt"))