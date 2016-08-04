(ns euler.sandbox)

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