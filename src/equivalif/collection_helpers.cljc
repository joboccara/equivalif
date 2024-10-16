(ns equivalif.collection-helpers 
  (:require [clojure.core :refer [read-string]]
            [clojure.string :as string]))

(defn slice [coll begin-included end-excluded]
  (take (- end-excluded begin-included) (drop begin-included coll)))

(defn replace-slice [coll begin-included end-excluded new-slice]
  (if (= (+ begin-included 1) end-excluded) coll
      (concat (take begin-included coll)
              new-slice
              (drop end-excluded coll))))

(defn gsub [coll old-sub new-sub]
  (let [coll-as-string (pr-str coll)
        old-sub-as-string (subs (subs (pr-str old-sub) 0 (dec (count (pr-str old-sub)))) 1)
        new-sub-as-string (subs (subs (pr-str new-sub) 0 (dec (count (pr-str new-sub)))) 1)]
    (read-string (string/replace coll-as-string old-sub-as-string new-sub-as-string))))

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
