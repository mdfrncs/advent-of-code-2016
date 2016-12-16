(ns advent-of-code-2016.day-12
  (:require [clojure.string :as str]
            [advent-of-code-2016.util :as util]))

(defn extract
  [state val-or-reg]
  (if (integer? val-or-reg)
    val-or-reg
    (get state val-or-reg)))

(defmulti invoke (fn [name & args] name))

(defmethod invoke :cpy
  [_ state val-or-reg to-reg]
  (-> state
      (assoc to-reg (extract state val-or-reg))
      (update :index inc)))

(defmethod invoke :jnz
  [_ state val-or-reg steps]
  (let [s (extract state val-or-reg)]
    (if (= s 0)
      (update state :index inc)
      (update state :index + steps))))

(defmethod invoke :inc
  [_ state reg]
  (-> state
      (update reg inc)
      (update :index inc)))

(defmethod invoke :dec
  [_ state reg]
  (-> state
      (update reg dec)
      (update :index inc)))

(defn execute-line
  [state [fn args]]
  (apply invoke fn state args))

(defn compile-line
  [line]
  (let [segments (str/split line #" ")
        fn (keyword (first segments))
        args (mapv #(if-let [x (util/to-int %)]
                      x
                      (keyword %))
                   (rest segments))]
    [fn args]))

(defn solve
  [input registers]
  (let [lines (into [] (comp (map str/trim)
                             (map compile-line))
                    (str/split-lines input))]
    (loop
      [state (assoc registers :index 0)]
      (if (< (:index state) (count lines))
        (recur (execute-line state (nth lines (:index state))))
        state))))

(defn solve-pt1
  [input]
  (:a (solve input {:a 0 :b 0 :c 0 :d 0})))

(defn solve-pt2
  [input]
  (:a (solve input {:a 0 :b 0 :c 1 :d 0})))

(def input (slurp "resources/day_12.txt"))