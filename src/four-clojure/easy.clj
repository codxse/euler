(ns four-clojure.easy
  (:require [clojure.set :as set])
  (:use [clojure.repl]))

;; 19. last element
(defn new-last
  "Return last element of sequence."
  [coll]
  (first (reverse coll)))

(= (new-last [1 2 3 4 5]) 5)

(= (new-last '(5 4 3)) 3)

(= (new-last ["b" "c" "d"]) "d")

;; 20. penultimate element
(defn before-last
  "Return second element from last of sequence."
  [coll]
  (second (reverse coll)))

(= (before-last (list 1 2 3 4 5)) 4)

(= (before-last ["a" "b" "c"]) "b")

(= (before-last [[1 2] [3 4]]) [1 2])

;; 21. nth element
(defn new-nth
  "Return nth element from sequences."
  [coll e]
  (get (into [] coll) e))

(defn recur-nth
  [coll e]
  (if (= e 0)
    (first coll)
    (recur (rest coll) (dec e))))

(= (new-nth '(4 5 6 7) 2) 6)

(= (new-nth [:a :b :c] 0) :a)

(= (new-nth [1 2 3 4] 1) 2)

(= (new-nth '([1 2] [3 4] [5 6]) 2) [5 6])

;; 22. count a sequence
(defn new-count
  "Count element of sequence."
  [coll]
  (let [s (seq coll)]
    (if (empty? s)
      0
      (+ 1 (new-count (rest s))))))

(= (new-count '(1 2 3 3 1)) 5)

(= (new-count "Hello World") 11)

(= (new-count [[1 2] [3 4] [5 6]]) 3)

(= (new-count '(13)) 1)

(= (new-count '(:a :b :c)) 3)

;; 23. reverse a sequence
(defn new-reverse
  "Return reversed coll."
  ([coll] (new-reverse coll '()))
  ([coll reverse-coll]
   (if (empty? coll)
     reverse-coll
     (recur (rest coll)
            (conj (seq reverse-coll) (first coll))))))

(= (new-reverse [1 2 3 4 5]) [5 4 3 2 1])

(= (new-reverse (sorted-set 5 7 2 7)) '(7 5 2))

(= (new-reverse [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

;; 24. sum it up
(defn sum
  "Sum it up."
  [coll]
  (reduce + coll))

(= (sum [1 2 3]) 6)

(= (sum (list 0 -2 5 5)) 8)

(= (sum #{4 2 1}) 7)

(= (sum '(0 0 -1)) -1)

(= (sum '(1 10 3)) 14)

;; 25. find odd number
(defn filter-odd
  "Return odd list with odd value only."
  [coll]
  (filter odd? coll))

(= (filter-odd #{1 2 3 4 5}) '(1 3 5))

(= (filter-odd [4 2 1 6]) '(1))

(= (filter-odd [2 2 4 6]) '())

(= (filter-odd [1 1 1 3]) '(1 1 1 3))

;; 27. palindrome detector
(defn palindrome?
  "Return true if palindrome."
  [coll]
  (= (seq coll) (seq (reverse coll))))

(false? (palindrome? '(1 2 3 4 5)))

(true? (palindrome? "racecar"))

(true? (palindrome? [:foo :bar :foo]))

(true? (palindrome? '(1 1 3 3 1 1)))

(false? (palindrome? '(:a :b :c)))