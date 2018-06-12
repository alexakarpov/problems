(ns problems.codefights)

(defn h [y a]
  (if (and
       (<= y 100)
       (>= y 0))
    (inc a)
    (h (- y 100) (inc a))))

(defn y->c [y]
  (h y 0))

(y->c 1701)

(defn checkPalindrome [inputString]
  (let [r (apply str (reverse inputString))]
    (= inputString r)))

(checkPalindrome "qweewq")

(defn adjacentElementsProduct [inputArray]
  (let [pairs (map vec (partition 2 1 inputArray))
        _ (println pairs)]
    (apply max (map #(* (first %) (second %)) pairs))))

(adjacentElementsProduct [1 2 3 4 5 6])

(apply max [1 2 3 4])
