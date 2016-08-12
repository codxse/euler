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

;; problem no. 2
(defn fib-next
  "Generate element of fibonachi sequence."
  [coll]
  (let [f (first coll)
        l (last coll)]
    [l (+ f l)]))

(defn fibonachi
  "Generate lazy fibonachi sequence."
  [coll]
  (map first (iterate fib-next coll)))

(= 4613732
   (reduce +
           (filter even?
                   (take-while #(< % (* 4 1000 1000))
                               (fibonachi [1 2])))))

;; problem no. 3
(defn sqrt-ceil
  "Let sqrt of 2 is 1.413...,
  sqrt-ceil will round up 1.413... to 2."
  [x]
  (int (Math/ceil (Math/sqrt x))))

(defn prime?
  "Return true if x is prime."
  ([x] (prime? x (sqrt-ceil x)))
  ([x iterator]
   (cond
     (= 2 x) true
     (<= x 1) false
     (= 1 iterator) true
     (zero? (rem x iterator)) false
     :else (recur x (dec iterator)))))

(defn prime-factor
  "Return sequence of prime factor of x."
  [x]
  (let [s (sqrt-ceil x)
        p (filter prime? (range 1 s))]
    (filter #(zero? (rem x %)) p)))

(= 6857 (last (prime-factor 600851475143)))
