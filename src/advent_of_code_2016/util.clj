(ns advent-of-code-2016.util)

(defn to-int
  [x]
  (try
    (Integer/parseInt (str x))
    (catch Exception e)))

(defn to-long
  [x]
  (try
    (Long/parseLong (str x))
    (catch Exception e)))
