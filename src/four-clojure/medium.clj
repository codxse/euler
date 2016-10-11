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

(defn n59 juxt)

(= [21 6 1] ((n59 + max min) 2 3 5 1 6 4))

(= ["HELLO" 5] ((n59 #(.toUpperCase %) count) "hello"))

(= [2 6 4] ((n59 :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

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
        g #(if (zero? (rem % %2))
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