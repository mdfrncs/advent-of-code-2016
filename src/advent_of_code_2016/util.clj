(ns advent-of-code-2016.util)

(defn to-int
  [x]
  (try
    (Integer/parseInt (str x))
    (catch Exception e)))
