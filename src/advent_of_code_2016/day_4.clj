(ns advent-of-code-2016.day-4
  (:require [clojure.string :as str]))

(defn checksum
  [filtered]
  (->> (frequencies filtered)
       (sort (fn [[a x] [b y]]
               (if (= x y)
                 (compare a b)
                 (compare y x))))
       (map first)
       (take 5)
       (map str)
       str/join))

(defn decode
  [line num]
  (-> (sequence
        (comp (map int)
              (map #(- % 97))
              (map #(+ % num))
              (map #(mod % 26))
              (map #(+ % 97))
              (map char)
              (map str))
        line)
      str/join))

(defn real?
  [input]
  (let [m (re-matcher #"(.*)-([0-9]+)\[([a-z]*)\]" input)
        [s l num code] (re-find m)
        num-as-int (Integer/parseInt num)
        line (filter #(not (= \- %)) (str/trim l))
        chksm (checksum line)]
    (if (= code chksm)
      [(decode line num-as-int) num-as-int]
      [nil 0])))

(defn solve
  [input]
  (let [lines (map str/trim (str/split-lines input))]
    (transduce (comp (map real?)
                     (map last)) + 0 lines)))

(defn solve-pt2
  [input]
  (let [lines (map str/trim (str/split-lines input))
        room "northpoleobjectstorage"]
    (first (sequence (comp (map real?)
                           (filter #(= room (first %)))
                           (map last)) lines))))

(def input (slurp "resources/day_4.txt"))