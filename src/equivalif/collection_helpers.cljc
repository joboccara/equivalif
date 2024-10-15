(ns equivalif.collection-helpers)

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (if (= (+ begin-included 1) end-excluded) coll
      (concat (take begin-included coll)
              new-slice
              (drop end-excluded coll))))

(defn remove-around-value
  ([coll around-values-set]
   (let [positions (keep-indexed #(when (contains? around-values-set %2) %1) coll)]
     (remove-around-value '() positions coll)))
  ([_ positions coll]
   (if (empty? positions) coll
       (let [position (last positions)
             newline-token {:type :variable, :name "\n"}
             begin (if (and (> position 0) (= (nth coll (- position 1)) newline-token))
                     (- position 1)
                     position)
             end-included (if (and (< position (- (count coll) 1)) (= (nth coll (+ position 1)) newline-token))
                   (+ position 1)
                   position)]
         (recur '() (butlast positions) (replace-slice coll begin (+ end-included 1) [(nth coll position)]))))))
