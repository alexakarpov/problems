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

(defn almostIncreasingSequence [s]
  (let [strictly-increasing?
        (fn [xs]
          (let [a (reduce (fn [[prev acc n] x]
                            (let [res (and acc (< prev x))]
                              (if res [x res (inc n)]
                                  (reduced [false n]))))
                          [(first xs) true 0]
                          (rest xs))]
               (if (= 3 (count a)) true a)))
        check (strictly-increasing? s)]
    (if (= 1 (count check)) true
        (let [remove-n (fn [v n] (concat (subvec v 0 n) (subvec v (inc n))))
              r1 (remove-n s (second check))
              r2 (remove-n s (inc (second check)))
              a1 (strictly-increasing? r1)
              a2 (strictly-increasing? r2)]
          (if (true? a1) true (true? a2))))))


(defn allLongestStrings [inputArray] 
  (let [len (reduce #(max %1 (count %2)) 0 inputArray)
        _ (println len)]
    (filter #(= len (count %)) inputArray)))


(defn char-seq->map [s]
  (reduce (fn [acc c]
            (if (contains? acc c)
              (update-in acc [c] inc)
              (assoc acc c 1)))
          {}
          s))

(defn commonCharacterCount [s1 s2]
  (let [[m o]
        (reduce (fn [[acc-in acc-out] c]
                  (if (or (nil? (acc-in c))
                          (zero? (acc-in c)))
                    [acc-in acc-out]
                    [(update-in acc-in [c] dec)
                     (conj acc-out c)]))
                [(char-seq->map s2) []]
                s1)]
    (count o)))
