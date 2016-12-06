(ns advent-of-code-2016.day_1
  (:require [clojure.string :as str]))

(let [compass [:n :e :s :w]]
  (defn turn
    [facing turning]
    (let [change (case turning :L -1 :R 1)]
      (get compass (mod (+ (.indexOf compass facing) change) 4)))))

(defn add-distance
  [{:keys [facing traveled]} v]
  (let [distance (Integer/parseInt (.substring v 1))
        turning (keyword (str (first v)))
        new-facing (turn facing turning)]
    {:facing   new-facing
     :traveled (update traveled new-facing + distance)}))

(defn solve [input]
  (let [segments (str/split input #", *")
        {:keys [n e s w]} (:traveled (reduce add-distance {:facing   :n
                                                           :traveled {:n 0 :e 0 :s 0 :w 0}} segments))]
    (+ (Math/abs (- n s))
       (Math/abs (- e w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-segment
  [{:keys [facing positions solution] :as entry} _]
  (if solution
    entry
    (let [last (last positions)
          new-pos (case facing
                    :n (update last :x + 1)
                    :s (update last :x - 1)
                    :e (update last :y + 1)
                    :w (update last :y - 1))
          solution (some #{new-pos} positions)]
      {:facing    facing
       :positions (conj positions new-pos)
       :solution  solution})))

(defn add-positions
  [{:keys [facing positions solution] :as entry} v]
  (if solution
    entry
    (let [distance (Integer/parseInt (.substring v 1))
          turning (keyword (str (first v)))
          now-facing (turn facing turning)
          processed-segment (reduce add-segment {:facing    now-facing
                                                 :positions positions} (range distance))]
      {:facing    now-facing
       :positions (:positions processed-segment)
       :solution  (:solution processed-segment)})))


(defn solve-pt2 [input]
  (let [segments (str/split input #", *")
        {:keys [x y]} (:solution (reduce add-positions {:facing    :n
                                                        :positions [{:x 0 :y 0}]} segments))]
    (+ (Math/abs x)
       (Math/abs y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def input
  "R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3")