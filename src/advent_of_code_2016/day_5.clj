(ns alia-kata.d5
  (:require [digest :as di]
            [clojure.string :as str]
            [advent-of-code-2016.util :as util]))

(defn zero-hash?
  [input]
  (let [hash (di/md5 input)]
    (when (.startsWith hash "00000")
      [(nth hash 5) (nth hash 6)])))

(defn hashes
  [input]
  (sequence (comp (map #(str input %))
                  (map zero-hash?)
                  (filter some?))
            (range)))

(defn solve-pt1
  [input]
  (->> (hashes input)
       (take 8)
       (map first)
       str/join))

;;;;;;;;;;;;;;;;;
;; Part 2

(defn update-solution
  [soln x]
  (let [[str-index e] x
        idx (util/to-int str-index)]
    (if (and idx
             (< idx 8)
             (not (get soln idx)))
      (assoc soln idx e)
      soln)))

(defn solved?
  [soln]
  (= 8 (count soln)))

(defn solve-pt2
  [input]
  (loop [soln {}
         seq (hashes input)]
    (let [x (first seq)
          updated-soln (update-solution soln x)]
      (if (solved? updated-soln)
        (str/join (map #(get updated-soln %) (range 8)))
        (recur updated-soln
               (rest seq))))))

(def input "ugkcyxxp")

