(ns active.quickcheck2.arbitrary-test
  (:require [clojure.test :refer :all]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer [generate choose-int]]
            [active.quickcheck2.tree :as tree]
            [active.quickcheck2.arbitrary :refer :all]))

(def test-gen (random/make-random-generator 12))

(defn test-generate [m] (generate 5 test-gen m 20))

(deftest arbitrary-sequence-like-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like list
                                                              choose-int)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-sequence-like list
                                                            choose-int)))))))
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              choose-int)))))
    (is (every? vector? (take 100 (tree/to-list
                                   (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              choose-int)))))))))

(deftest arbitrary-list-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-list choose-int)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-list choose-int)))))))
    (is (every? (partial every? int?) (take 100 (tree/to-list
                                                 (test-generate
                                                  (arbitrary-generator
                                                   (arbitrary-list choose-int)))))))))
