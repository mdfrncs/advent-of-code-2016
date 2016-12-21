(ns advent-of-code-2016.day-19)

(defprotocol ElfProtocol
  (left! [this left])
  (right! [this right])
  (left [this])
  (right [this]))

(defrecord Elf
  [id left right]
  ElfProtocol
  (left! [this new-left] (swap! left (fn [_] new-left)))
  (right! [this new-right] (swap! right (fn [_] new-right)))
  (left [this] @left)
  (right [this] @right))

(defn elf
  [id]
  (->Elf id (atom nil) (atom nil)))

(defn elves
  [num]
  (let [first-elf (elf 1)
        last-elf (reduce (fn [last-elf id]
                           (let [elf (elf (+ id 2))]
                             (left! last-elf elf)
                             (right! elf last-elf)
                             elf))
                         first-elf
                         (range (dec num)))]
    (left! last-elf first-elf)
    (right! first-elf last-elf)
    first-elf))

(defn remove-elf
  [elf]
  (let [left-elf (left elf)
        right-elf (right elf)]
    (right! left-elf right-elf)
    (left! right-elf left-elf)))

(defn solve-pt1
  [input]
  (loop [elf (elves input)]
    (remove-elf (left elf))
    (if (= elf (left elf))
      (get elf :id)
      (recur (left elf)))))

(defn get-elf
  [last-idx last-elf idx count]
  (loop [curr last-idx
         elf last-elf]
    (if (= curr idx)
      elf
      (recur (mod (inc curr) count)
             (left elf)))))

(defn solve-pt2
  [input]
  (loop [idx 0
         last-idx 0
         last-elf (elves input)
         count input]
    (if (= 1 count)
      (:id last-elf)
      (let [opposite-idx (int (mod (+ (/ count 2) idx) count))
            opposite-elf (get-elf last-idx last-elf opposite-idx count)
            next-idx (if (> opposite-idx idx) (inc idx) idx)]
        (recur (mod next-idx (dec count))
               opposite-idx
               (remove-elf opposite-elf)
               (dec count))))))

(def input 3014603)