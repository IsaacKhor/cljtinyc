(ns cljtinyc.core
  (:require [clojure.pprint :refer [pprint]]
            [cljtinyc.lex-spec :as lex]
            [clojure.tools.cli :as cli]
            [cljtinyc.parse-spec :as parse])
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
    
(defn scan-source [options source-path]
  (println "Scanning source from:" source-path)
  (let [source (slurp source-path)
        tokens (lex/c-lexer source)
        show (:show-lex options)]
    (if show (pprint tokens))
    tokens))

(defn parse-tree [options tokens]
  (println "Getting the parse tree...")
  (let [show (:show-parse-tree options)
        parse-tree (parse/parse-a1 tokens)]
    ; (pprint parse-tree)
    (if show (pprint parse-tree))
    parse-tree))

(defn -main
  [& args]
  (let [parsed (cli/parse-opts args cli-options)
        {:keys [options arguments summary errors]} parsed]
    (when (:help options)
      (do (println (generate-summary summary))
          (System/exit 0)))
    (when errors
      (do (println "Arguments invalid. Use -h --help for help." errors)
          (System/exit 1)))
    (let [[source-path] arguments
          tokens (scan-source options source-path)
          parse-tree (parse-tree options tokens)]
      parse-tree)))
