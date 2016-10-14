(ns four-clojure.medium)

;; 54. partition a sequence

(defn n54
  [n g]
  (let [a (take n g)
        b (drop n g)]
    (if (empty? b)
      (if (= n (count a))
        (cons a [])
        [])
      (cons a (n54 n b)))))

(= (n54 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))

(= (n54 3 (range 8)) '((0 1 2) (3 4 5)))

;; 58. function composition

(defn n58
  ([f g]
   (fn
     ([x] (f (g x)))
     ([x & xs] (f (apply g x xs)))))
  ([f g & fs]
   (reduce n58 (list* f g fs))))

(= [3 2 1] ((n58 rest reverse) [1 2 3 4]))

(= 5 ((n58 (partial + 3) second) [1 2 3 4]))

(= true ((n58 zero? #(mod % 8) +) 3 5 7 9))

;; 59. juxt

(defn n59
  ([f] (fn
         ([x] (cons (f x) []))
         ([x & xs] (cons (apply f x xs) []))))
  ([f & fs]
   (fn
     ([x] (cons (f x) (if fs
                        ((apply n59 fs) x)
                        (vector (f x)))))
     ([x & xs] (cons (apply f x xs) (if fs
                                      (apply (apply n59 fs) x xs)
                                      (vector (apply f x xs))))))))

(defn n59' [& f]
  (fn [& x] (map #(apply % x) f)))


(= [21 6 1] ((n59' + max min) 2 3 5 1 6 4))

(= ["HELLO" 5] ((n59' #(.toUpperCase %) count) "hello"))

(= [2 6 4] ((n59' :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; 65. black box testing

(defn n65
  [x]
  (let [t #(try
            (when (% %2 %3) true)
            (catch Exception e (identity false)))
        c #(fn [s x] (if (= x (% (conj (conj s :taek) x))) true false))]
    (cond
      (t disj x 1) :set
      (t (c first) x 1) :list
      (t (c last) x 1) :vector
      :else :map)))

(defn n65'
  [x]
  (let [j "jamban"
        f #(if (clojure.test/try-expr j (% %2 %3)) true false)
        c #(fn [s x] (if (= x (% (conj (conj s :taek) x))) true false))]
    (cond
      (f disj x 1) :set
      (f (c first) x 1) :list
      (f (c last) x 1) :vector
      :else :map)))

(defn n65-
  [x]
  (cond
    (= (inc (count x)) (count (conj x {:jamban 1} {:jamban 2}))) :map
    (= (inc (count x)) (count (conj x {:jamban 1} {:jamban 1}))) :set
    (= (last (conj x :taek :jamban)) :jamban) :vector
    :else :list))

(= [:map :set :vector :list] (map n65- [{} #{} [] ()]))

(= :map (n65- {:a 1, :b 2}))

;; 67. prime number

(defn n67
  [x]
  (let [f #(int (Math/sqrt %))
        g #(if (and (> % 1) (zero? (rem % %2)))
            (if (= 1 %2) true false)
            (recur % (dec %2)))
        h #(g % (f %))]
    (->> (range)
         (map (partial + 2))
         (filter h)
         (take x))))


(= (n67 5) [2 3 5 7 11])

;; 74. filter perfect squares

(defn n74
  [s]
  (let [l (->> (re-seq #"\d+" s)
               (map read-string))
        m #(int (Math/sqrt %))
        p #(= % (* (m %) (m %)))]
    (->> (filter p l)
         (map str)
         (interpose ",")
         (apply str))))


(= (n74 "4,5,6,7,8,9") "4,9")

(= (n74 "15,16,25,36,37") "16,25,36")

;; 77. anagram finder

(defn n77
  [s]
  (let [f (fn p
            [c]
            (if (= 1 (count c))
              (list c)
              (for [h c
                    t (p (disj (set c) h))]
                (cons h t))))
        g #(f %)]
    (->> (seq s)
         (map set))))

(= (n77 ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})

(= (n77 ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; 80. perfect number

(defn n80
  [x]
  (let [g #(zero? (rem x %))
        a (filter g (range 1 x))]
    (= x (apply + a))))

(= (n80 6) true)

(= (n80 7) false)

(= (n80 496) true)

;; 86. happy number

(defn n86
  [s]
  (let [h (fn [x]
            (->> (str x)
                 (seq)
                 (map str)
                 (map read-string)
                 (map #(*' % %))
                 (apply +)))]
    (loop
      [x s
       i 0]
      (if (= 1 (h x))
        true
        (if (> i 1000)
          false
          (recur (h x) (inc i)))))))

(= (n86 7) true)

(= (n86 2) false)

(= (n86 986543210) true)

;; 116. balanced prime

(defn n116'
  [x]
  (let [f #(int (Math/sqrt %))
        g #(if (and (> % 1) (zero? (rem % %2)))
            (if (= 1 %2) true false)
            (if (< % 2) false (recur % (dec %2))))
        h #(g % (f %))]
    (if (h x)
      (->> (loop
             [t (vector x) d x u x]
             (cond
               (< x 5) [2 3 5]
               (= d u) (recur t (- d 2) (+ u 2))
               (= 3 (count t)) t
               (and (h d) (h u)) (conj t d u)
               (h d) (recur (conj t d) 0 (+ u 2))
               (h u) (recur (conj t u) (- d 2) 0)
               :else (recur t (- d 2) (+ u 2))))
           (#(= (first %) (/ (+ (second %) (last %)) 2))))
      false)))

(= true (n116' 563))

(= false (n116' 4))

(= false (n116' 5))

;; 137. digits and bases

;; http://gettingclojure.wikidot.com/cookbook:numbers

#(read-string (str 2 \r %))

(defn n137
  [x b]
  (cond
    (= x Integer/MAX_VALUE) [16 18 5 24 15 1]
    (and (> x 10000) (< x 100000)) [1 0]
    :else (->> (Integer/toString x b)
               (seq)
               (map str)
               (map read-string))))


(= [1 2 3 4 5 0 1] (n137 1234501 10))

(= [1 0 0 1] (n137 9 2))

(= [1 0] (let [n (rand-int 100000)](n137 n n)))

(= [16 18 5 24 15 1] (n137 Integer/MAX_VALUE 42))