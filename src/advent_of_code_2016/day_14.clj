(ns advent-of-code-2016.day-14
  (:require [digest :as di]))

(def md5
  (memoize (fn [salt input]
             (di/md5 (str salt input)))))

(def frequent-chars
  (memoize (fn [salt input hash-fn]
             (let [hash (hash-fn salt input)]
               (into #{} (comp (filter #(> (count %) 4))
                               (map first))
                     (partition-by identity hash))))))

(defn repeated-5?
  [salt input c hash-fn]
  (contains? (frequent-chars salt input hash-fn) c))


(defn key?
  [salt input c hash-fn]
  (loop [i input]
    (if (repeated-5? salt i c hash-fn)
      true
      (when (< i (+ 999 input))
        (recur (inc i))))))

(defn first-triplet-char
  [input]
  (first (first (filter #(> (count %) 2) (partition-by identity input)))))

(defn next-pad-key
  [salt input hash-fn]
  (loop [i input]
    (let [hash (hash-fn salt i)
          c (first-triplet-char hash)]
      (if (and c
               (key? salt (inc i) c hash-fn))
        [hash i]
        (recur (inc i))))))


(defn solve
  [input hash-fn]
  (loop [i -1
         num 0
         hashes []]
    (if (= num 64)
      i
      (let [[hash index] (next-pad-key input (inc i) hash-fn)]
        (recur index (inc num) (conj hashes [hash index]))))))


(defn solve-pt1
  [input]
  (solve input md5))

(def stetched-hash
  (memoize (fn [key index]
             (let [hash (md5 key index)]
               (loop [i 0
                      hash-of-hash (di/md5 hash)]
                 (if (= i 2015)
                   hash-of-hash
                   (recur (inc i) (di/md5 hash-of-hash))))))))

(defn solve-pt2
  [input]
  (solve input stetched-hash))

(def input "yjdafjpo")