(ns euler.level-1
  (:require [clojure.set :as set]))

;; problem no. 1
(defn drop-before
  "Drop n element of collection coll, before first element of e."
  [e coll]
  (if (= (first coll) e)
    coll
    (recur e (rest coll))))

(defn gen35
  "Generate multiple 3 or five below maxim."
  [maxim]
  (let [three (range 0 maxim 3)
        five (range 0 maxim 5)]
    (rest (sort (distinct (set/union three five))))))

(defn gen35?
  "Return true if e is multiple of 3 or 5."
  [e]
  (if (or (= 0 (rem e 3))
          (= 0 (rem e 5)))
    true
    false))

(= 233168 (reduce + (gen35 1000)))
(= 233168 (reduce + (filter gen35? (range 1 1000))))