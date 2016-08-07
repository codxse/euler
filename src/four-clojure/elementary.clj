(ns four-clojure.elementary
  (:require [clojure.string :as string])
  (:require [clojure.set :as set])
  (:use [clojure.repl]))

;; 1. nothing but truth
(= true true)

;; 2. simple math
(= (- 10 (* 2 3)) 4)

;; 3. intro into string
(= "HELLO WORLD" (.toUpperCase "hello world"))

;; 4. intro into lists
(= (list :a :b :c) '(:a :b :c))

;; 5. lists: conj
(= '(1 2 3 4) (conj '(2 3 4) 1))

(= '(1 2 3 4) (conj '(3 4) 2 1))

;; 6. intro into vectors
(= [:a :b :c]
   (list :a :b :c)
   (vec '(:a :b :c))
   (vector :a :b :c))

;; 7. vectors: conj
(= [1 2 3 4] (conj [1 2 3] 4))

(= [1 2 3 4] (conj [1 2] 3 4))

;; 8. intro into sets
(= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))

(= #{:a :b :c :d} (set/union #{:a :b :c} #{:b :c :d}))

;; 9. sets: conj
(= #{1 2 3 4} (conj #{1 4 3} 2))

;; 10. intro to maps
(= 20 ((hash-map :a 10, :b 20, :c 30) :b))

(= 20 (:b {:a 10, :b 20, :c 30}))

;; 11. maps: conj
(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

;; 12. intro into sequences
(= 3 (first '(3 2 1)))

(= 3 (second [2 3 4]))

(= 3 (last (list 1 2 3)))

;; 13. sequences: rest
(= [20 30 40] (rest [10 20 30 40]))

;; 14. intro to function
(= 8 ((fn add-five [x] (+ x 5)) 3))

(= 8 ((fn [x] (+ x 5)) 3))

(= 8 (#(+ % 5) 3))

(= 8 ((partial + 5) 3))

;; 15. double down
(= (* 2 20) 40)

;; 16. hello world
(= (#(str "Hello, " % "!") "Dave") "Hello, Dave!")

(= (format "Hello, %s!" "Dave") "Hello, Dave!")

;; 17. sequences: map
(= '(6 7 8) (map #(+ % 5) '(1 2 3)))

;; 18. sequences: filter
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

;; 35. local bindings
(= 7 (let [x 5] (+ 2 x)))

(= 7 (let [x 3, y 10] (- y x)))

(= 7 (let [x 21] (let [y 3] (/ x y))))

;; 36. let it be
(= 10 (let [x 7
            y 3
            z 1]
        (+ x y)))

;; 37. regex
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; 52. intro to destructuring
(= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))

;; 57. simple recursion
(= '(5 4 3 2 1)
   ((fn foo
      [x]
      (when (> x 0)
        (conj (foo (dec x)) x)))
     5))

;; 64. intro into reduce
(= 15 (reduce + [1 2 3 4 5]))

(=  0 (reduce + []))

(=  6 (reduce + 1 [2 3]))

;; 68. recurring theme
(= (map #(+ 2 %) [5 4 3 2 1])
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

;; 71. rearranging code
(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
   5)

;; 72. rearranging code: ->>
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)

;; 134. a nil key
(defn key-nil?
  [ky mp]
  (and (contains? mp ky)
       (nil? (mp ky))))

(true?  (key-nil? :a {:a nil :b 2}))

(false? (key-nil? :b {:a nil :b 2}))

(false? (key-nil? :c {:a nil :b 2}))

;; 156. map defaults

(defn map-default
  [v coll]
  (reduce #(conj %1 %2) (map #(hash-map % v) coll)))

(defn map-simple
  [v coll]
  (into {} (map #(hash-map % v) coll)))

(= (map-simple 0 [:a :b :c]) {:a 0 :b 0 :c 0})

(= (map-simple "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})

(= (map-simple [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})

;; 161. subset and superset
(set/superset? #{2 1} #{2})

(set/subset? #{1} #{2 1})

(set/superset? #{2 1} #{1 2})

(set/subset? #{1 2} #{2 1})