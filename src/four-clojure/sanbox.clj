(ns four-clojure.sandbox)

(= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])


(defn g
  [s]
  (let [o (flatten [0 s])
        p (partition-all 2 1 o)
        q (map #(apply + %) p)]
   (cons s (lazy-seq (g q)))))

[[0 3] [3 1] [1]]

(map #(apply + %) [[0 3] [3 1] [1]])