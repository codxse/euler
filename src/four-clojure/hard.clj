(ns four-clojure.hard)

;; 53

(defn n53
  [[x & xs]]
  (if (nil? xs)
    []
    (if (= (first xs) (inc x))
      (cons [x (first xs)] (n53 xs))
      (n53 (next xs)))))

(= (n53 [1 0 1 2 3 0 4 5]) [0 1 2 3])

(= (n53 [5 6 1 3 2 7]) [5 6])

(= (n53 [2 3 3 4 5]) [3 4 5])

(= (n53 [7 6 5 4]) [])

(defn n53
  [xs]
  (loop [t []
         a (inc (first xs))
         b (second xs)
         s (drop 2 xs)]
    (if s
      (if (= a b)
        (recur (conj t a b)
               (first s)
               (second s)
               (drop 2 s))
        (recur t
               (first s)
               (second s)
               (drop 2 s)))
      t)))



