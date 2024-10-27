(ns equivalif.collection-helpers)

(declare sub-positions)

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (if (= (+ begin-included 1) end-excluded) coll
      (concat (take begin-included coll)
              new-slice
              (drop end-excluded coll))))

(defn gsub
  ([coll old-sub new-sub] (gsub (sub-positions coll old-sub) coll old-sub new-sub))
  ([positions coll old-sub new-sub]
   (if (empty? positions) coll
       (let [position (last positions)]
         (recur (butlast positions)
                (replace-slice coll position (+ position (count old-sub)) new-sub)
                old-sub
                new-sub)))))

(defn sub-positions
  ([coll sub] (sub-positions 0 [] coll sub))
  ([begin positions coll sub]
   (if (= begin (count coll)) positions
       (let [sub-length (count sub)
             end-excluded (+ begin sub-length)]
         (if (= (slice coll begin end-excluded) sub)
           (recur end-excluded (conj positions begin) coll sub)
           (recur (inc begin) positions coll sub))))))

(defn remove-around-value [coll around-value center-value]
  (gsub (gsub coll
              (list around-value center-value)
              (list center-value))
        (list center-value around-value)
        (list center-value)))

(defn remove-if-index [pred coll]
  (keep-indexed #(when ((complement pred) %1) %2) coll))
