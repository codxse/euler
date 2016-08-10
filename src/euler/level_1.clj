(ns euler.level-1
  (:require [clojure.set :as set]))

;; problem no. 1
(defn drop-before
  [e coll]
  (if (= (first coll) e)
    coll
    (recur e (rest coll))))

(defn gen35
  [maxim]
  (let [three (range 0 maxim 3)
        five (range 0 maxim 5)]
    (rest (sort (distinct (set/union three five))))))

(= 233168 (reduce + (gen35 1000)))