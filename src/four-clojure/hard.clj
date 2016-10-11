(ns four-clojure.hard)

;; 53. longest increasing sub-seq

(defn n53
  [s]
  (->> (partition 2 1 s)
       (partition-by #(< (first %) (last %)))
       (map flatten)
       (map distinct)
       (filter #(< (first %) (last %)))
       (reduce #(if (>= (count %) (count %2)) % %2) [])))

(= (n53 [1 0 1 2 3 0 4 5]) [0 1 2 3])

(= (n53 [5 6 1 3 2 7]) [5 6])

(= (n53 [2 3 3 4 5]) [3 4 5])

(= (n53 [7 6 5 4]) [])