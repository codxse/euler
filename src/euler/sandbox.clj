(ns euler.sandbox
  (:require [clojure.set :as set])
  (:use [clojure.string :only [join]]))

(def fac1
  "Contoh penamaan function anonimus."
  (fn [n]
    (if (= n 1)
      1
      (* n (fac1 (- n 1))))))

(defn fac2
  "Contoh penamaan function on-the-fly."
  [n]
  (if (= 1)
    1
    (* n (fac2 (- n 1)))))

(defn messanger
  "Contoh penggunaan let.
  Let nge-bind value immutable.
  value bisa atomic atau expresi.
  Sifatnya lexical scope."
  [msg]
  (let [prime 7
        odd 3
        is-even (even? msg)]
    (println prime odd is-even)))

(defn hello-arity
  "Clojure support no-args,
  default value bisa di set."
  ([] (println "This is default value"))
  ([msg] (println msg)))

(defn n-args
  "Atau bisa juga n buah argument.
  Disebut juga disebut variadic function.
  & symbil di parameter,
  setelah & direpresentasikan sequence.
  Contoh: (n-args :Budi :temannya :Ani :Doni :Tika)."
  [this these & those]
  (println this these those))

(defn apply-list
  "Given a function, apply those function
  to collection of arguments and then unpack it."
  [this these & those]
  (apply print this these those))


;;;;;;;;;;;;;;;;;;
;;; namespaces ;;;
;;;;;;;;;;;;;;;;;;

(require 'clojure.set)

(clojure.set/union #{1 2 3} #{1 2})

;; or

(require '[clojure.set :as set])

(set/union #{1 2 3} #{1 2})

(require '[clojure.string :as str :only [join]])

;;;;;;;;;;;;;;;;;;;
;;; collections ;;;
;;;;;;;;;;;;;;;;;;;

;; list

(list 1 2 3)

(quote (1 2 3))

'(1 2 3)

(conj '(1 2) 0)

;; vector

[1 2 3]

(vector 1 2 3)

(def vectr (vec '(1 2 3)))

(nth [1 2 3] 0)

(conj vectr '(12 13))

;; map

{}

{:a 1 :b 2}

;; access by its key
(:a {:a 1 :b 2})

({:a 1 :b 2} :a)

;; add another key-val
(assoc {:a 1} :b 2)

;; pop by key
(dissoc {:a 1} :a)

;; convert vector to map
(conj {} [:a 1])

;; nested map

(def jdoe {:name "Jhon Doe" :address {:zip 44185}})

;; get value by key
(get-in jdoe [:address :zip])

;; update value by its key
(assoc-in jdoe [:address :zip] 40135)

;; instead update the value, update-in update with
;; function that passed
(update-in jdoe [:address :zip] inc)

;; set

#{}

#{:a :b}

(conj #{} :a)

;; evaluate to true
(contains? #{:a} :a)

(require '[clojure.set :as set])

(set/union #{:a} #{:b})

(set/difference #{:a :b} #{:a})

(set/intersection #{:a :b :c} #{:d :b :e})

;;;;;;;;;;;;;;;;;;;;;
;;; destructuring ;;;
;;;;;;;;;;;;;;;;;;;;;

(def stuff [1 2 3 4 5])

;; bind a b c to first three value in stuff
(let [[a b c] stuff]
  (list (+ a b) (+ b c)))

;; bind a b c d e f to stuff
(let [[a b c d e f] stuff]
  (list d e f))

(let [[a & others] stuff]
  (println a)
  (println others))

(def m {:a 1 :b 2})

(let [{:keys [a b]} m]
  [a b])

(let [{:keys [a b c]} m]
  [a b c])

(defn draw-point
  [& {:keys [a b c]
      :or {a 0 b 0 c 0}}]
  [a b c])

;;;;;;;;;;;;;;;;;;;;
;;; sequence api ;;;
;;;;;;;;;;;;;;;;;;;;

(seq [1 2 3])

(first (seq [1 2 3]))

(rest (seq [1 2 3]))

(cons 1 (rest (seq [1 2 3])))

;; fibonachi sequence

(defn gen-fib
  "generate fibonachi sequences."
  [a b]
  (map first
       (iterate
         #(into [] [(second %) (+ (first %) (second %))])
         [a b])))

(def fibs
  (map first
       (iterate
         (fn [[a b]] [b (+ a b)])
         [0 1])))

;;;;;;;;;;;;;;;;;;;;;
;;; controll flow ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn show-evens
  [coll]
  (if-let [evens (seq (filter even? coll))]
    (println (str "the evens are: " evens))
    (println "there were no evens.")))

(defn grather-than-10?
  [x]
  (cond
    (> x 10) (str x " is grather then 10")
    (> x 5) (str x " is grather then 5")
    :else "no man sky."))

(defn is10p?
  [x]
  (condp = x
    10 (str x " is 10")
    5 (str x " is 5")
    "no man sky."))

(defn is10c?
  [x]
  (case x
    10 (str x " is 10")
    5 (str x " is 5")
    "no man sky."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recurssion and itteration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iterate over sequence
;; similiar to java foreach
;; if laze-seq doseq force evaluation
(doseq [n (range 3)]
  (println n))

;; evaluate expression n times
(dotimes [i 3]
  (println i))

;; list comperhenssion, not for-loop
;; generator function for sequence permutation
(for [x [0 1]
      y [0 1]]
  [x y])

;; functional looping construct
;; loop defines bindings
;; recur re-excecutes loop with new binding
(loop [i 0]
  (if (< i 10)
    (recur (inc i))
    i))

(defn to-ten
  [i]
  (if (< i 10)
    (recur (inc i))
    i))

(defn factorial
  ([n] (factorial 1 n))
  ([accum n]
   (if (zero? n)
     accum
     (recur (*' accum n) (dec n)))))