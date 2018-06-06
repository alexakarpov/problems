(ns problems.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn atoi [a-seq]
  {:pre [(coll? a-seq) ]}
  "function to convert a list of digits to an integer number they represent"
  (let [codes (map int a-seq)
        valid-seq (filter #(and (>= % 48)
                                (<= % 57))
                          codes)
        convert #(- (int %) 48)
        ]
    (if (= (count valid-seq)
           (count codes))
      (int (apply + (map-indexed (fn [idx value]
                                   (let [len (count a-seq)
                                         p10 (dec (- len idx))]
                                     (* (Math/pow 10 p10) value)))
                                 (map convert a-seq))))
      :error)))

(defn is-palindrome? [^java.lang.String s]
  "Tests a string for whether it is a palindrome"
  (let [len (count s)]
    (if (or (= 0 len) (= 1 len))
      true
      (and (= (nth s 0) (nth s (dec len)))
           (recur (take (dec (dec len)) (drop 1 s)))))))

(defn buy-sell [stocks]
  "Given a collection of integer numbers, representing the price of the stock on consecutive days, shows which buy and sell days would yield maximum profit."
  (loop [buy-candidate {:buy 1
                        :price (first stocks)}
         deal {:buy 1
               :sell 1
               :profit 0}
         day 1
         stocks stocks]
    (if (empty? stocks) deal
        (let [price (first stocks)
              profit (- price (:price buy-candidate))]
          (recur (if (< price (:price buy-candidate))
                   {:day day :price price}
                   buy-candidate)
                 (if (> profit (:profit deal))
                   (merge buy-candidate {:sell day
                                         :profit profit})
                   deal)
                 (inc day)
                 (rest stocks))))))

(defrecord ListNode [value next])

(defn linked-list->vector [linked-list]
  (loop [ll linked-list
         acc []]
    (if (nil? ll)
      acc
      (let [{:keys [value next]} ll]
        (recur next (conj acc value))))))

(defn i-to-ll [llist i]
  (ListNode. i llist))

(defn vector->linked-list [avec]
  (reduce i-to-ll nil (reverse avec)))

(defn reverseNodesInKGroups [l k]
  (let [avec (linked-list->vector l)
        new-list (partition k k [nil] avec)
        last-k (reverse (remove nil? (last new-list)))
        ]
    (mapcat reverse
            (if (= k (count last-k))
              new-list
              (concat (butlast new-list) (list last-k))))))

(reverseNodesInKGroups (vector->linked-list [1, 2, 3, 4 5]) 3)
