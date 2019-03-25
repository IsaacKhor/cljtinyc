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
  {:end-of-file #{:end-of-file},
   :F            #{:lparen :id},
   :lparen       #{:lparen},
   :star         #{:star},
   :T            #{:lparen :id},
   :rparen       #{:rparen},
   :Ep           #{:plus :epsilon},
   :E            #{:lparen :id},
   :id           #{:id},
   :plus         #{:plus},
   :epsilon      #{:epsilon},
   :Tp           #{:star :epsilon}})

(deftest first-set-test
  (testing "First set calculation"
    (is (= (llp/calculate-first-sets test-grammar)
           grammar-first))))