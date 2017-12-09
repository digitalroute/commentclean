(ns commentclean.core-test
  (:require [clojure.test :refer :all]
            [commentclean.core :refer :all]))


(defn verify-newlines [x1 x2]
  (is (= (count x1) (count x2)))
  (map (fn [[v1 v2]] (is (= (count v1) (count v2)))) x1 x2))
  

(defn verify-comment [uncommented-text commented-text]
  (is (= (count commented-text) (count uncommented-text)))
  (verify-newlines (.split uncommented-text "\n") (.split commented-text "\n"))
  (is (= uncommented-text (clean commented-text))))

(deftest verify-clean-comment
  (verify-comment "" "")
  (verify-comment "1 / 2" "1 / 2")
  (verify-comment "1" "1")
  (verify-comment "12   " "12 //")
  (verify-comment "12       " "12 //asdf")
  (verify-comment "12         " "12 /*asdf*/")
  (verify-comment "12       \n  " "12 /*asdf\n*/")
  (verify-comment "12 \"anders\"" "12 \"anders\"")
  (verify-comment "1\n2\n3" "1\n2\n3")
  (verify-comment "1\n  \n\n\n  2\n3" "1\n/*\n\n\n*/2\n3")
  (verify-comment "123            456" "123 /* **** */ 456")
)
