(ns equivalif.collection-helpers)

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (if (= (+ begin-included 1) end-excluded) coll
      (concat (take begin-included coll)
              new-slice
              (drop end-excluded coll))))

(defn remove-newlines-around-block-delimiters
  ([tokens]
   (let [positions (keep-indexed #(when (contains? #{:open-block :close-block} (:type %2)) %1) tokens)]
     (remove-newlines-around-block-delimiters positions tokens)))
  ([positions tokens]
   (if (empty? positions) tokens
       (let [position (last positions)
             newline-token {:type :variable, :name "\n"}
             begin (if (and (> position 0) (= (nth tokens (- position 1)) newline-token))
                     (- position 1)
                     position)
             end-included (if (and (< position (- (count tokens) 1)) (= (nth tokens (+ position 1)) newline-token))
                   (+ position 1)
                   position)]
         (recur (butlast positions) (replace-slice tokens begin (+ end-included 1) [(nth tokens position)]))))))
