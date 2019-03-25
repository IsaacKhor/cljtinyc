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
