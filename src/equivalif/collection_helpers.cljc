(ns equivalif.collection-helpers)

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (if (= (+ begin-included 1) end-excluded) coll
      (concat (take begin-included coll)
              new-slice
              (drop end-excluded coll))))

(defn remove-around-value
  ([coll around-value center-values-set]
   (let [positions (keep-indexed #(when (contains? center-values-set %2) %1) coll)]
     (remove-around-value '() positions coll around-value)))
  ([_ positions coll around-value]
   (if (empty? positions) coll
       (let [position (last positions)
             begin (if (and (> position 0) (= (nth coll (- position 1)) around-value))
                     (- position 1)
                     position)
             end-included (if (and (< position (- (count coll) 1)) (= (nth coll (+ position 1)) around-value))
                   (+ position 1)
                   position)]
         (recur '() (butlast positions)
                (replace-slice coll begin (+ end-included 1) [(nth coll position)])
                around-value)))))
