(ns cljtinyc.parser
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]))

(defn parser-from-file [grammar-path]
  (insta/parser 
    (slurp grammar-path)
    :output-format :hiccup
    :auto-whitespace :standard))

(def a3-parser (parser-from-file "resources/grammar.ebnf"))

(defn ^:private expr-trans 
  "Transform a + b + c to (+ a (+ b c))"
  [& expr]
  (let [op (second expr)
        left (first expr)
        right (vec (drop 2 expr))]
    ; If op is nil, then we only have 1 element
    (if (nil? op)
      left
      ; Handle binary and unary operations differently
      (if (empty? right)
        ; Unary expression come in the form 'op arg' instead of 'arg op arg'
        [:UnaryOp left op] 
        [:BinaryOp op left (apply expr-trans right)]))))

(def ^:private transformation
  {:Number #(Integer/parseInt %)
   :Expression expr-trans
   :E2 expr-trans
   :E3 expr-trans})

(defn cst-to-ast
  "Converts the parse tree to an AST"
  [ast]
  (insta/transform transformation ast))
    
; For REPL use

(defn test-grammar [source]
  (let [parser (parser-from-file "resources/grammar.ebnf")
        tree (parser source)]
    ; (pprint tree)
    (second tree)))

(def code (slurp "resources/a5.c"))
(def cst (test-grammar code))
(def ast (cst-to-ast cst))

