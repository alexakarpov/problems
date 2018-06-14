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


(defn any-pairs-overlap? [ps]
  (let [recer (fn [[b ov] v]
                (and b (< ov v)))]
    (reduce recer [true -1] ps)))

(any-pairs-overlap? [[1 2] [3 4] [4 7]])

(defn almostIncreasingSequence [s]
  (let [ps (partition 2 1 s)
        _ (println ps)
        cleared (filter #(< (first %) (second %)) ps)
        _ (println cleared)]
    (or (= (count cleared) (count ps))
        (and
         (< (- (count ps) (count cleared)) 2)
         
         ;; after clearing the dip itself, what about the climbing back up steps?
         ))))


(assert (= false
           (almostIncreasingSequence [1 3 2 1])))

(assert (= false (almostIncreasingSequence [1 2 1 2])))

(assert (= false (almostIncreasingSequence [1, 2, 3, 4, 5, 3, 5, 6])))
