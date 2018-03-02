(ns problems.core-test
  (:require [clojure.test :refer :all]
            [problems.core :refer :all]))

(deftest atoi-test
  (testing "atoi works"
    (is (= 1234567890 (atoi [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0])))
    (is (= :error (atoi [\1 \2 \x])))
    (is (= 42 (atoi [\0 \4 \2])))))

