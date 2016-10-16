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

;; 28. flatten a sequence
(defn flatten2
  [col]
  (if (coll? col)
    (mapcat flatten2 col)
    [col]))


(= (flatten2 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))

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

;; 43. reverse interleave

(defn n43
  [s n]
  (->> (partition n s)
       (apply map vector)))


(= (n43 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))

;; 44. rotate sequence

(defn n44
  [n s]
  (let [p (rem n (count s))
        q (+ (count s) p)
        c concat]
    (if (pos? p)
      (c (drop p s) (take p s))
      (c (drop q s) (take q s)))))

(= (n44 2 [1 2 3 4 5]) '(3 4 5 1 2))

(= (n44 -2 [1 2 3 4 5]) '(4 5 1 2 3))

;; 46. flipping out
(defn n46
  [f]
  (fn [x y]
    (f y x)))


(= 3 ((n46 nth) 2 [1 2 3 4 5]))

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

;; 55. cout occurrences

(defn n55
  [s]
  (let [m (group-by identity s)
        k (keys m)
        v (map #(count %) (vals m))]
    (zipmap k v)))

(= (n55 [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

;; 56. find distinct item

(defn n56
  [s]
  (let [h (fn [x e]
            (if (some #(= e %) x)
              x
              (conj x e)))]
    (reduce h [] s)))

(= (n56 [1 2 1 3 1 2 4]) [1 2 3 4])

(= (n56 '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))

;; 61. map contruction
(defn map-con
  "Return map construction"
  [coll1 coll2]
  (apply hash-map (interleave coll1 coll2)))

(= (map-con [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})

;; 62. re-implement iterate
(defn iter2
  [f x]
  (lazy-seq (cons x
                  (iter2 f (f x)))))

(= (take 5 (iter2 #(* 2 %) 1)) [1 2 4 8 16])

(= (take 100 (iter2 inc 0)) (take 100 (range)))

(= (take 9 (iter2 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;; 63. group a sequence
(defn grup-by2
  [f coll]
  (let [keys (distinct (map f coll))
        n (count keys)]
    (->> (for [k keys
               c coll
               :when (= k (f c))]
           c)
         (partition-by f)
         (interleave keys)
         (apply hash-map))))


(= (grup-by2 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

(= (grup-by2 count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

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

;; 88. symetric difference
(defn symdiff
  [set1 set2]
  (set/difference
    (set/union set1 set2)
    (set/intersection set1 set2)))

(= (symdiff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

(= (symdiff #{:a :b :c} #{}) #{:a :b :c})

(= (symdiff #{} #{4 5 6}) #{4 5 6})

(= (symdiff #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

;; 90. cartesian product
(defn cartesian-product
  "Return cartesian product from two list."
  [col1 col2]
  (set (for [c1 col1
             c2 col2]
         [c1 c2])))

(= (cartesian-product #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

;; 95. to tree or not tree

(defn tree?
  [s]
  (let [fs (vec (flatten s))]
    (if (contains? fs (.indexOf fs false))
      false
      (#(odd? (count %)) fs))))

(tree? [1 [2 [3 [4 false nil] nil] nil] nil])

;; 96. beauty is symmetry

(defn n96 [[_ l r]]
  (if (not (coll? l))
    true
    (if (not (and (coll? r) (coll? r)))
      false
      (let [[lv ll lr] l
            [rv rl rr] r]
        (and (= lv rv) (n96 [nil ll rr]) (n96 [nil rl lr]))))))


;; 97. pascal triangle
(defn pascal
  [x]
  (iterate (fn
             [y]
             [(last y) (+ (first y)
                          (last y))]) x))

(map count (vals (group-by identity (for [x [1 2]
                                          y [1 2]
                                          z [1 2]]
                                      (+ x y z)))))

(->> (for [x [1 2]
           y [1 2]
           z [1 2]
           a [1 2]]
       (+ x y z a))
     (group-by identity)
     (vals)
     (map count))

(defn nk
  [vec-of-nk]
  (let [fak (fn [n]
              (let [s (map inc (range n))]
                (apply * s)))]
    (quot (fak (first vec-of-nk))
          (* (fak (second vec-of-nk))
             (fak (- (first vec-of-nk)
                     (second vec-of-nk)))))))

(defn pascal
  [i]
  (let [nk (fn nk
             [vec-of-nk]
             (let [fak (fn [n]
                         (let [s (map inc (range n))]
                           (apply * s)))]
               (quot (fak (first vec-of-nk))
                     (* (fak (second vec-of-nk))
                        (fak (- (first vec-of-nk)
                                (second vec-of-nk)))))))
        vn (take i
                 (map #(vec [(dec i) %])
                      (range)))]
    (map nk vn)))


(= (map pascal (range 1 6))
   [[1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])

(= (pascal 11)
   [1 10 45 120 210 252 210 120 45 10 1])

;; 99. product digit
(defn n99
  [x y]
  (->> (* x y)
       str seq
       (map str)
       (map read-string)))

(= (n99 999 99) [9 8 9 0 1])

;; 100. lcm
(defn lcm
  [a b]
  (let [gcd (fn
              [x y]
              (if (zero? y)
                x
                (recur y (rem x y))))]
    (/ (* a b)
       (gcd a b))))

(defn lcm2
  [& rest]
  (reduce lcm rest))

(defn lcm3
  [& rest]
  (let [lcm  (fn
              [a b]
              (let [gcd (fn
                          [x y]
                          (if (zero? y)
                            x
                            (recur y (rem x y))))]
                (/ (* a b)
                   (gcd a b))))]
    (reduce lcm rest)))

(defn lcm4
  [& rest]
  (let [g (fn [x y]
              (if (zero? y)
                x
                (recur y (rem x y))))]
    (reduce #(/ (* %1 %2)
                (g %1 %2)) rest)))

(== (lcm4 2 3) 6)

(== (lcm4 5 3 7) 105)

(== (lcm4 7 5/7 2 3/5) 210)

(== (lcm4 1/3 2/5) 2)

(== (lcm4 3/4 1/6) 3/2)

(defn gcd
  "Return GCD from two number."
  [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

;; 107. simple closures
(defn ho
  [power]
  #(int (Math/pow % power)))

(= [1 8 27 64] (map (ho 3) [1 2 3 4]))

(= 256 ((ho 2) 16),
   ((ho 8) 2))

(= [1 2 4 8 16] (map #((ho %) 2) [0 1 2 3 4]))

;; 118. re-implement map
(defn map2
  [f coll]
  (if (seq coll)
    (cons (f (first coll))
          (lazy-seq (map2 f (next coll))))))

;; not pass unit test
(defn map3
  [f coll]
  (loop
    [c coll
     t []]
    (if (empty? c)
      t
      (recur (rest c)
             (conj t (f (first c)))))))

;; not pas unit test
(defn map4
  [f coll]
  (let
    [g (fn
         [f c]
         (cons (f (first c))
               (lazy-seq (map4 f (rest c)))))
     n (count coll)]
    (take n (g f coll))))

(= [3 4 5 6 7]
   (map2 inc [2 3 4 5 6]))

(= (repeat 10 nil)
   (map2 (fn [_] nil) (range 10)))

(= [1000000 1000001]
   (->> (map2 inc (range))
        (drop (dec 1000000))
        (take 2)))

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

;; 122. read binary number
(defn bin2dec
  [binary-string]
  (let [binary-seq (->> (seq binary-string)
                        (map str)
                        (map read-string)
                        reverse)
        base (fn [x] (if (zero? x) 0 2))]
    (->> (map #(* (Math/pow (base %1) %2)
                  %1)
              binary-seq (range))
         (reduce +)
         int)))

(= 0 (bin2dec "0"))

(= 7 (bin2dec "111"))

(= 8 (bin2dec "1000"))

(= 1365 (bin2dec "10101010101"))

;; 135. infix calculator
(defn infix-clac
  "Return result of operation from infix notation."
  [n f & re]
  (if (empty? re)
    n
    (recur (f n (first re))
           (second re)
           (drop 2 re))))

(= 42 (infix-clac 38 + 48 - 2 / 2))

(= 72 (infix-clac 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))

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

;; 146. trees into tables

(defn n146
  [m]
  (let [f (fn [m]
            (let [k (first (keys m))
                  v (vals m)
                  kv (apply concat (map keys v))
                  vv (apply concat (map vals v))]
              (map (fn [x y z]
                     [[x y] z]) (repeat k) kv vv)))]
    (->> (concat (map key m) (map val m))
         (partition 2)
         (#(if (= 1 (count %)) (partition-all 1 (first %)) %))
         (apply map (fn [k v] {k v}))
         (map f)
         (apply concat)
         (into {}))))

(defn n146'
  [m]
  (->> (for [mp m
             vl (val mp)
             :let [ky (key vl)
                   v (val vl)]]
         (hash-map [(key mp) ky] v))
       (apply merge)))

(= (n146 '{a {p 1, q 2}
           b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})

(= (n146 '{[1] {a b c d}
           [2] {q r s t u v w x}})
   '{[[1] a] b, [[1] c] d,
     [[2] q] r, [[2] s] t,
     [[2] u] v, [[2] w] x})

(= (n146 '{m {1 [a b c] 3 nil}})
   '{[m 1] [a b c], [m 3] nil})

;; 147. pascal's trapezoid

(defn pascal-trapezoid
  [coll]
  (lazy-seq (cons coll
                  (pascal-trapezoid
                    (let [c (concat [0] coll [0])]
                      (map #(+' % %2) c (rest c)))))))

(= (take 2 (pascal-trapezoid [3 1 2])) [[3 1 2] [3 4 3 2]])

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

;; 173. intro to destructuring 2

(= 3
   (let [[x y] [+ (range 3)]] (apply x y))
   (let [[[x y] b] [[+ 1] 2]] (x y b))
   (let [[x y] [inc 2]] (x y)))
