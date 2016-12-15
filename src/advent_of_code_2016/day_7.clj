(ns advent-of-code-2016.day-7
  (:require [clojure.string :as str]))

(defn partition-substrings
  [input size]
  (mapcat #(partition size (.substring input %)) (range size)))

(defn abba?
  [[a b c d]]
  (and a b c d
       (not (= a b))
       (= a d)
       (= b c)))

(defn has-abba?
  [segment]
  (let [sequences (partition-substrings segment 4)]
      (seq (filter abba? sequences))))

(defn tls?
  [line]
  (let [segments (str/split line #"[\[\]]")
        supernet-segments (take-nth 2 segments)
        hypernet-segments (take-nth 2 (rest segments))]

    (and (seq (filter has-abba? supernet-segments))
         (not (seq (filter has-abba? hypernet-segments))))))

(defn solve
  [input]
  (let [lines (map str/trim (str/split-lines input))]
    (count (filter tls? lines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn aba?
  [[a b c]]
  (and a b c
       (= a c)
       (not (= a b))))

(defn bab?
  [[a b c] [x y z]]
  (and (= a c y)
       (= b x z)))

(defn bab-finder
  [abas]
  (fn [x]
    (and (aba? x)
         (seq (filter #(bab? % x) abas)))))

(defn ssl?
  [line]
  (let [segments (str/split line #"[\[\]]")
        supernet-segments (mapcat #(partition-substrings % 3) (take-nth 2 segments))
        hypernet-segments (mapcat #(partition-substrings % 3) (take-nth 2 (rest segments)))
        abas (into #{} (filter aba? supernet-segments))]

    (seq (filter (bab-finder abas) hypernet-segments))))

(defn solve-pt2
  [input]
  (let [lines (map str/trim (str/split-lines input))]
    (count (filter ssl? lines))))

(def input (slurp "resources/day_7.txt"))