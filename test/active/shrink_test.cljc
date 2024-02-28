(ns active.quickcheck2.shrink-test
    (:require [clojure.test :refer :all]
            [active.clojure.monad :as monad]
            [active.quickcheck2.generator :as generator]
            [active.quickcheck2.generator-applicative :refer [integrated]]
            [active.quickcheck2.tree :as tree]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer [generate]]
            [clojure.math.numeric-tower :refer [expt]])
  (:use active.quickcheck2))

(def test-gen (random/make-random-generator 12))

(defn test-generate [m] (generate 5 test-gen m 20))

(defn is-counterexample
  [mresult]
  (not (check-result-ok (generate mresult))))

(defn get-counterexample
  [mresult]
  (map second (check-result-arguments-list (generate mresult))))

(def t-0 (tree/lazy-tree [0] []))
(def t-12 (tree/lazy-tree [12] []))
(def t-5 (tree/lazy-tree [5] []))
(def t-3 (tree/lazy-tree [3] []))
(def t-1 (tree/lazy-tree [1] []))
(def t-13 (tree/lazy-tree [13] []))
(def t-14 (tree/lazy-tree [14] []))
(def t-2 (tree/lazy-tree [2] []))
(def t-4 (tree/lazy-tree [4] []))
(def t-6 (tree/lazy-tree [6] []))
(def t-7 (tree/lazy-tree [7] []))
(def t-0 (tree/lazy-tree [0] []))
(def t-8 (tree/lazy-tree [8] []))
(def t-9 (tree/lazy-tree [9] []))
(def tree-list (tree/lazy-tree ['(1 2 3)] []))

(deftest find-failing-finds-nothing
  (testing "if the vector of shrunks contains no counterexample find-failing returns :no-failing-result"
    (is (= :no-failing-result (test-generate (find-failing [] (partial < 13)))))
    (is (= :no-failing-result (test-generate (find-failing [t-12] (partial < 5)))))
    (is (= :no-failing-result (test-generate (find-failing [t-5 t-3 t-1] (partial < 0)))))
    (is (= :no-failing-result (test-generate (find-failing [t-12 t-13 t-14] (partial > 20)))))
    (is (= :no-failing-result (test-generate (find-failing [tree-list] list?))))))

(deftest find-failing-finds-result
  (testing "if the vector of shrunks contains at least one  counterexample it returns the first one"
    (is (= [t-1 (make-check-result false [] [] #{})]
           (test-generate (find-failing [t-1] (partial = 3)))))
    (is (= [t-2 (make-check-result false [] [] #{})]
           (test-generate (find-failing [t-3 t-4 t-2 t-8 t-2] (partial < 2)))))
    (is (= [tree-list (make-check-result false [] [] #{})]
           (test-generate (find-failing [tree-list] (fn [x] (< (count x) 2))))))))

(defn is-counterexample
  [mresult]
  (not (check-result-ok (test-generate mresult))))

(defn get-counterexample
  [mresult]
  (second (first (first (check-result-arguments-list (test-generate mresult))))))

(defn numshrink
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(defn listshrink
  [xs]
  (if (empty? xs) [] [(rest xs)]))

(deftest for-all-with-shrink-with-name-counterexample
   (testing "for-all-with-shrink-with-name shrinks an counterexample"
     (is (= [1] (get-counterexample (for-all-with-shrink-with-names (partial = 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0] (get-counterexample (for-all-with-shrink-with-names (partial > 0)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [1] (get-counterexample (for-all-with-shrink-with-names (partial = 0)
                                                                     "x"
                                                                     [(integrated numshrink (monad/return 5))]))))
     (is (= [3] (get-counterexample (for-all-with-shrink-with-names (partial > 3)
                                                                   ["x"]
                                                                   [(integrated numshrink (monad/return 5))]))))
     (is (= [0 1] (get-counterexample (for-all-with-shrink-with-names (partial =)
                                                                     ["x" "y"]
                                                                     [(integrated numshrink (monad/return 5))
                                                                      (integrated numshrink (monad/return 6))]))))
     (is (= [[3]] (get-counterexample
                   (for-all-with-shrink-with-names empty?
                                                   ["x" ]
                                                   [(integrated listshrink (monad/return [1 2 3]))]))))))

(defn numshrinkv [[x]] (map vector (numshrink x)))
(defn listshrinkv [[xs]] (map vector (listshrink xs)))

(deftest shrinking-gives-counterexample
  (testing "if shrinking gets an counterexample it returns an counterexample"
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [2]) (partial = 0) 20)))
    (is (is-counterexample (shrinking ["y"] (tree/unfold numshrinkv [7]) (partial > 3) 20)))
    (is (is-counterexample (shrinking ["z"] (tree/unfold numshrinkv [3]) (partial < 5) 15)))
    (is (is-counterexample (shrinking ["v"] (tree/unfold numshrinkv [4]) (partial = 5) 11)))
    (is (is-counterexample (shrinking ["x"] (tree/unfold numshrinkv [4]) (partial = 3) 11)))
    (is (is-counterexample (shrinking ["xs"] (tree/unfold listshrinkv [[1 2 3]]) empty? 11)))))


(deftest such-that-maybe-lazy
  (testing "tree outcome on big tree from such-that-maybe is fast"
    (let [bignum 9999999999999999999]
      (tree/tree-outcome (test-generate (generator/such-that-maybe (generator/choose-integer 0 bignum) odd?))))))
