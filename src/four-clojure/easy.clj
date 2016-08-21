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

;; 33. replicate a seq
(defn repseq
  "Return replicate of seq."
  [coll n]
  (apply concat (map #(replicate n %) coll)))

(= (repseq [1 2 3] 2) '(1 1 2 2 3 3))

(= (repseq [:a :b] 4) '(:a :a :a :a :b :b :b :b))

(= (repseq [4 5 6] 1) '(4 5 6))

(= (repseq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))

(= (repseq [44 33] 2) [44 44 33 33])

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

;; 39. interleave two seq
(defn new-interleave
  "A function which takes two sequences
  and returns the first item from each,
  then the second item from each,
  then the third, etc."
  [coll1 coll2]
  (loop
    [c1 coll1
     c2 coll2
     temp []]
    (if (and c1 c2)
      (recur
        (next c1)
        (next c2)
        (conj temp
              (first c1)
              (first c2)))
      temp)))

(= (new-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))

(= (new-interleave [1 2] [3 4 5 6]) '(1 3 2 4))

(= (new-interleave [1 2 3 4] [5]) [1 5])

(= (new-interleave [30 20] [25 15]) [30 25 20 15])

;; interpose a seq
(defn interps
  "A function exactly like interpose."
  [e coll]
  (butlast (loop
             [e e
              c coll
              t []]
             (if c
               (recur e
                      (next c)
                      (conj t (first c) e))
               t))))

(defn interps2
  "A function exactly like interpose."
  [e coll]
  (butlast (interleave coll (repeat e))))

(= (interps 0 [1 2 3]) [1 0 2 0 3])

(= (apply str (interps ", " ["one" "two" "three"])) "one, two, three")

(= (interps :z [:a :b :c :d]) [:a :z :b :z :c :z :d])

;; 41. drop every nth element
(defn rm-factor-idx
  "Return seq from droped every nth element."
  [coll factor]
    (remove #(zero?(rem
                     (inc (.indexOf coll %))
                     factor))
            coll))

(= (rm-factor-idx [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])

(= (rm-factor-idx [:a :b :c :d :e :f] 2) [:a :c :e])

(= (rm-factor-idx [1 2 3 4 5 6] 4) [1 2 3 5 6])

;; 42. factorial fun
(defn factorial
  "Return factorial of n."
  [n]
  (if (= n 1)
    n
    (* n (factorial (dec n)))))

(defn factorial2
  "Return factorial of n."
  [n]
  (->> (inc n)
       (range 1)
       (reduce *)))

(defn factorial21
  "Return factorial of n"
  [n]
  (reduce * (range 1 (inc n))))

(= (factorial2 1) 1)

(= (factorial2 3) 6)

(= (factorial2 5) 120)

(= (factorial2 8) 40320)

;; 49. split a sequence
(defn split-seq
  "Return two part of seq, splited by n"
  [n coll]
  (conj []
        (take n coll)
        (drop n coll)))

(= (split-seq 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])

(= (split-seq 1 [:a :b :c :d]) [[:a] [:b :c :d]])

(= (split-seq 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

;; 61. map contruction
(defn map-con
  "Return map construction"
  [coll1 coll2]
  (apply hash-map (interleave coll1 coll2)))

(= (map-con [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})

;; 66. gcd
(defn gcd
  "Return GCD from two number."
  [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

(= (gcd 2 4) 2)

(= (gcd 10 5) 5)

(= (gcd 5 7) 1)

(= (gcd 1023 858) 33)

;; 81. set intersection
(defn intersec
  "Return intersection from two set"
  [set1 set2]
  (set (for [s1 set1
            s2 set2
            :when (= s1 s2)]
         s1)))

(= (intersec #{0 1 2 3} #{2 3 4 5}) #{2 3})

(= (intersec #{0 1 2} #{3 4 5}) #{})

(= (intersec #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; 83. half truth
(defn half-truth
  "Return true if some of the parameters are true,
  but not all of the parameters are true."
  [& a]
  (if (every? true? a)
    false
    (if (some true? a)
      true
      false)))

(= false (half-truth false false))

;; 90. cartesian product
(defn cartesian-product
  "Return cartesian product from two list."
  [col1 col2]
  (set (for [c1 col1
             c2 col2]
         [c1 c2])))

(= (cartesian-product #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

;; 99. product digit
(defn n99
  [x y]
  (->> (* x y)
       str seq
       (map str)
       (map read-string)))

(= (n99 999 99) [9 8 9 0 1])

;; 120. sum of equare of digits
(defn num2digit
  "Return seq of digit from number.
  ex. 120 -> (1 2 0)."
  [x]
  (->> (str x)
       (seq)
       (map str)
       (map read-string)))

(defn sqdigit
  "Return sequared number of seq.
  ex. (1 2 0) -> (1 4 0)."
  [x]
  (reduce + (map #(* % %)
                 (num2digit x))))

(defn st-sqdigit?
  "Return true if x is smaller than square digit.
  ex. 10 -> false, coz (< 10 (+ (sqrt 1) (sqrt 0)))."
  [x]
  (if (< x (sqdigit x))
    true
    false))

(defn n120
  "Return count element of st-digit?."
  [col]
  (count (filter st-sqdigit? col)))

(defn long120
  "Return count element of st-digit?."
  [col]
  (let [num2digit (fn [x]
                    (->> (str x)
                         (seq)
                         (map str)
                         (map read-string)))
        sqdigit (fn [x]
                  (->> (num2digit x)
                       (map #(* % %))
                       (reduce +)))
        st-sqdigit? (fn [x]
                      (if (< x (sqdigit x))
                        true
                        false))]
    (count (filter st-sqdigit? col))))

(= 8 (n120 (range 10)))

(= 19 (n120 (range 30)))

(= 50 (n120 (range 100)))

(= 50 (n120 (range 1000)))

;; 143. dot product
(defn dot-product
  "Return dot product from two collections."
  [col1 col2]
  (->> (map * col1 col2)
       (apply +)))

(defn dot-product2
  "Recrusive dot product."
  [col1 col2]
  (loop
    [c1 col1
     c2 col2
     p 0]
    (if (empty? c1)
      p
      (recur (rest c1)
             (rest c2)
             (+ p (* (first c1)
                     (first c2)))))))

(= 0 (dot-product [0 1 0] [1 0 0]))

(= 3 (dot-product [1 1 1] [1 1 1]))

;; 153. pairwise disjoint set
(defn disjoint?
  [set_]
  (let [set_ (seq set_)
        len (count set_)]
    (every? empty?
            (seq (for [s1 (range len)
                       s2 (range len)
                       :when (not= s1 s2)]
                   (set/intersection
                     (nth set_ s1)
                     (nth set_ s2)))))))

(= (disjoint? #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)

(= (disjoint? #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false)

(= (disjoint? #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)

(= (disjoint? #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true)

;; 157. indexing sequence
(defn n157
  "Return seq of e with their index."
  [col]
  (map list col (range)))

(= (n157 [:a :b :c]) [[:a 0] [:b 1] [:c 2]])

(= (n157 [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])

;; 166. comparisons
(defn compar
  [f x y]
  (cond
    (f x y) :lt
    (f y x) :gt
    :else :eq))


(= :gt (compar < 5 1))

(= :eq (compar (fn [x y] (< (count x) (count y))) "pear" "plum"))

(= :lt (compar (fn [x y] (< (mod x 5) (mod y 5))) 21 3))

(= :gt (compar > 0 2))