(ns cljtinyc.parser-test
  (:require [cljtinyc.llparser :as llp]
            [clojure.test :refer :all]))

(def test-grammar
  [[:E  :T :Ep]
   [:Ep :plus :T :Ep]
   [:Ep :epsilon]
   [:T  :F :Tp]
   [:Tp :star :F :Tp]
   [:Tp :epsilon]
   [:F  :lparen :E :rparen]
   [:F  :id]])

(def grammar-first
  {:E            #{:lparen :id},
   :Ep           #{:plus :epsilon},
   :T            #{:lparen :id},
   :Tp           #{:star :epsilon}
   :F            #{:lparen :id},
   :end-of-file  #{:end-of-file},
   :lparen       #{:lparen},
   :star         #{:star},
   :rparen       #{:rparen},
   :id           #{:id},
   :plus         #{:plus},
   :epsilon      #{:epsilon},})

(def grammar-follow
  {:E  #{:end-of-file :rparen}
   :Ep #{:end-of-file :rparen}
   :T  #{:end-of-file :rparen :plus}
   :Tp #{:end-of-file :rparen :plus}
   :F  #{:end-of-file :rparen :plus :star}})

(deftest first-follow-test
  (testing "First set calculation"
    (is (= (llp/calculate-first-sets test-grammar)
           grammar-first)))
  (testing "Follow set calculation"
    (is (= (llp/calculate-follow-sets test-grammar grammar-first)
           grammar-follow))))
