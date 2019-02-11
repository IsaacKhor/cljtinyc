(ns cljtinyc.lexer
  (:require [clojure.string :as str]
            [slingshot.slingshot :refer [throw+]]))
  ; (:import (dk.brics.automaton RegExp Automaton RunAutomaton State)))

(defn make-lexer [patterns]
  "Creates a lazy [string] -> [token] lexer from patterns"
  (fn [stream]))
    

(defn get-next-token [patmap linenum linestr prevcharnum]
  (let [; Trim whitespace since it doesn't matter in C
        trimmed-line (str/triml linestr)
        num-trimmed (- (count linestr) (count trimmed-line))
        ; Char count at start of token
        cur-char (+ num-trimmed prevcharnum)] 
    ; Try to match token one-by-one
    ; TODO: this is slow; try something more efficient later
    (loop [patterns patmap]
      (when (empty? patterns) 
        (throw+ {:type ::invalid-token
                 :linenum linenum
                 :char cur-char
                 :stage ::lexer
                 :msg (str "Failed to parse token :" trimmed-line)})) 
      (let [[tok-type tok-pat] (first patterns)
            match (re-find tok-pat trimmed-line)]
        (if match
          ; There's a match; calculate position and return everything else 
          (let [match-text (if (vector? match)
                             (get match 0)
                             match)
                match-length (count match-text)
                rest-line (subs trimmed-line match-length) 
                next-char (+ cur-char match-length)
                next-linenum (if (empty? rest-line)
                               (inc linenum)
                               linenum)]
            {:token {:type tok-type
                     :text match-text}
             :next-lex-state {:linenum next-linenum
                              :linestr rest-line
                              :charnum next-char}})
          ; No match; try another pattern
          (recur (rest patterns)))))))
    
