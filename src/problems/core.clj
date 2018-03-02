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
