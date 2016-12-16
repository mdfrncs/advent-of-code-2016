(ns advent-of-code-2016.day-15
  (:require [clojure.string :as str]
            [advent-of-code-2016.util :as util]))

(defn position
  [index {:keys [size position disc]}]
  (mod (+ position index disc) size))

(defn solution?
  [state index]
  (let [mapping-fn (partial position index)]
    (= (count state)
       (count (filter #(= 0 %) (map mapping-fn state))))))

(defn solve
  [discs]
  (first (filter (partial solution? discs) (range))))

(defn line->disc
  [line]
  (let [matcher (re-matcher #"Disc #([0-9]*) has ([0-9]*) positions; at time=0, it is at position ([0-9]*)."
                            line)
        [_ disc size position] (re-find matcher)]
    {:disc (util/to-int disc)
     :size (util/to-int size)
     :position (util/to-int position)}))

(defn solve-pt1
  [input]
  (let [lines (str/split-lines input)
        discs (mapv line->disc lines)]
    (solve discs)))

(defn solve-pt2
  [input]
  (let [lines (str/split-lines input)
        discs (mapv line->disc lines)]
    (solve (conj discs {:disc 7
                        :position 0
                        :size 11}))))

(def input (slurp "resources/day_15.txt"))

