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