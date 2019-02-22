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
    
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args)
        [source-path] arguments
        source (slurp source-path)]
    (println "Scanning the input...")
    (pprint (lex/c-lexer source))))
