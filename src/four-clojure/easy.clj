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