(ns equivalif.array-helpers)

(defn alternate
  "Returns an array that contains elements from both inputs alternatively, plus the remainder of the longest"
  [array1 array2]
  (cond (empty? array1) array2
        (empty? array2) array1
        :else (into [(first array1) (first array2)] (alternate (rest array1) (rest array2))))
  )
