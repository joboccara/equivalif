(ns equivalif.array-helpers)

(defn alternate
  "Returns an array that contains elements from both inputs alternatively, plus the remainder of the longest"
  ([array1 array2]
   (alternate [] array1 array2))
  ([result array1 array2]
   (cond (empty? array1) (into result array2)
         (empty? array2) (into result array1)
         :else (recur (into result [(first array1) (first array2)]) (rest array1) (rest array2))))
  )
