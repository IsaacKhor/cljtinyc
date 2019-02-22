(ns cljtinyc.core
  (:require [clojure.pprint :refer [pprint]]
            [cljtinyc.lex-spec :as lex]
            [clojure.tools.cli :as cli])
  (:gen-class))

(def cli-options
  [["-l" "--show-lex"        "Show the resulting lexical analysis"]
   ["-p" "--show-parse-tree" "Show the resulting parse tree"]
   ["-a" "--show-ast"        "Show the generated AST"]
   ["-h" "--help"]])

(defn- generate-summary [options-summary]
  (str 
    "CljTinyC: C99 subset compiler written in Clojure.

Usage: cljtinyc [-lpah] source_path
Options:\n"
     options-summary))
    
(defn scan-source [source-path]
  (println "Scanning source from: " source-path)
  (let [source (slurp source-path)]
    (pprint (lex/c-lexer source))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [parsed (cli/parse-opts args cli-options)
        {:keys [options arguments summary errors]} parsed]
    (if (:help options)
      (println (generate-summary summary))
      (System/exit 0))
    (if errors
      (println errors)
      (System/exit 1))
    (let [[source-path] arguments]
      (scan-source source-path))))
