(ns four-clojure.easy
  (:require [clojure.set :as set]
            [clojure.string :as str])
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

(defn rec-count
  "Count element using recur."
  ([coll] (rec-count (seq coll) 0))
  ([coll iterator]
   (if (empty? coll)
     iterator
     (recur (rest coll) (inc iterator)))))

(= (rec-count '(1 2 3 3 1)) 5)

(= (rec-count "Hello World") 11)

(= (rec-count [[1 2] [3 4] [5 6]]) 3)

(= (rec-count '(13)) 1)

(= (rec-count '(:a :b :c)) 3)

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

;; 26. fibonacci sequence
(defn fibo
  "Return n fibonacci sequence."
  [n]
  (take n (map second
               (iterate
                 (fn [x] [(last x)
                          (+ (first x)
                             (last x))])
                 [0 1]))))

(= (fibo 3) '(1 1 2))

(= (fibo 6) '(1 1 2 3 5 8))

(= (fibo 8) '(1 1 2 3 5 8 13 21))

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

;; 29. get the cap
(defn get-cap
  [word]
  (str/join (re-seq #"[A-Z]+" word)))

(= (get-cap "HeLlO, WoRlD!") "HLOWRD")

(empty? (get-cap "nothing"))

(= (get-cap "$#A(*&987Zf") "AZ")

;; 30. compress a sequence
(defn compress-seq
  "Function which removes consecutive duplicates from a sequence."
  [coll]
  (loop
    [c coll
     t []]
    (cond
      (empty? c) t
      (not= (first c) (last t)) (recur
                                  (rest c)
                                  (conj t (first c)))
      :else (recur
              (rest c)
              t))))

(defn compress-seq2
  "Function which removes consecutive duplicates from a sequence."
  [coll]
  (map first (partition-by identity coll)))

(= (compress-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))

(= (apply str (compress-seq "Leeeeeerrroyyy")) "Leroy")

(= (compress-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

;; 32. duplicate a sequence
(defn copy-seq
  "Return duplicate sequence."
  ([coll] (copy-seq coll []))
  ([coll temp]
    (if (empty? coll)
      temp
      (recur (rest coll) (conj temp
                               (first coll)
                               (first coll))))))

(defn copy-seq2
  "Return duplicate sequence."
  [coll]
  (sort (concat coll coll)))

(defn copy-seq3
  "Return duplicate sequence."
  [coll]
  (interleave coll coll))

(= (copy-seq [1 2 3]) '(1 1 2 2 3 3))

(= (copy-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))

(= (copy-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

(= (copy-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

;; 34. implement range
(defn new-range
  "Return sequence of range."
  [from to]
  (loop
    [start from
     end to
     temp []]
    (if (= start end)
      (seq temp)
      (recur (inc start)
             end
             (conj temp start)))))

(= (new-range 1 4) '(1 2 3))

(= (new-range -2 2) '(-2 -1 0 1))

(= (new-range 5 8) '(5 6 7))

;; 38. maximum value
(defn bf-max
  "Return max from seq."
  [& args]
  (reduce #(if (>= %1 %2) %1 %2) args))

(= (bf-max 1 8 3 4) 8)

(= (bf-max 30 20) 30)

(= (bf-max 45 67 11) 67)
