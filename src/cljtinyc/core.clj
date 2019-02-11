(ns cljtinyc.core
  (:require [instaparse.core :as ip]
            [clojure.pprint :refer [pprint]]
            [cljtinyc.lex-spec :as lex])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [[grammar-path source-path] args
        grammar (slurp grammar-path)
        source (slurp source-path)
        parser (ip/parser grammar :auto-whitespace :standard)]
    (println "Parsing the input...")
    (pprint (parser source))))
