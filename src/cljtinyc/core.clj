(ns cljtinyc.core
  (:require [clojure.pprint :refer [pprint]]
            [cljtinyc.lex-spec :as lex]
            [clojure.tools.cli :as cli]
            [cljtinyc.parser :as p]
            [cljtinyc.parse-spec :as parse]
            [cljtinyc.irgen :as ir]
            [cljtinyc.codegen :as cg])
  (:gen-class))

(def cli-options
  [;["-l" "--show-lex"        "Show the resulting lexical analysis"]
   ;["-p" "--show-parse-tree" "Show the resulting parse tree"]
   ["-a" "--show-ast"        "Show the generated AST"]
   ["-1" "--show-ir1"        "Show the first IR pass"]
   ["-2" "--show-ir2"        "Show the second IR pass (linear IR)"]
   ["-3" "--show-ir3"        "Show the third IR pass (MIPS asm)"]
   ["-m" "--show-asm"        "Show the output MIPS asm"]
   ["-h" "--help"]])

(defn- generate-summary [options-summary]
  (str 
    "CljTinyC: C99 subset compiler written in Clojure.

Usage: cljtinyc [-123ah] source_path
Options:\n"
     options-summary))
    
(defn compile [options source-path]
  (let [source-code (slurp source-path)
        cst (p/a3-parser source-code)
        ast (p/cst-to-ast cst)
        ir1 (ir/ir-pass1 ast)
        ir2 (ir/ir-pass2 ir1)
        ir3 (ir/ir-pass3 ir2)
        asm (cg/asm-generate @ir/symbol-table* ir3)]
    (when (:show-ast options) (pprint ast))
    (when (:show-ir1 options) (pprint ir1))
    (when (:show-ir2 options) (pprint ir2))
    (when (:show-ir3 options) (pprint ir3))
    (when (:show-asm options) (println asm))
    (spit "out.a" asm)
    asm))

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
    (let [[source-path] arguments]
      (compile options source-path))))
