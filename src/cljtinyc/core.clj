(ns cljtinyc.core
  (:require [clojure.pprint :refer [pprint]]
            [cljtinyc.lex-spec :as lex])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [[source-path] args
        source (slurp source-path)
        tokens (lex/c-lexer source)]
    (println "Scanning the input...")
    (pprint tokens)))
