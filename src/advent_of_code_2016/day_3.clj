(ns advent-of-code-2016.day-3)

(defn valid?
  [[a b c]]
  (and (< a (+ b c))
       (< b (+ a c))
       (< c (+ a b))))

(defn solve
  [input]
  (let [triangles (partition 3 input)]
    (count (filter valid? triangles))))

(defn solve-pt2
  [input]
  (let [horizontal (partition 3 input)
        columns (concat (map first horizontal)
                        (map second horizontal)
                        (map last horizontal))
        triangles (partition 3 columns)]
    (count (filter valid? triangles))))

(def input (load-string (str "[" (slurp "resources/day_3.txt") "]")))