(ns equivalif.collection-helpers 
  (:require [clojure.core :refer [read-string]]
            [clojure.string :as string]))

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
