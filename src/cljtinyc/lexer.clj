(ns cljtinyc.lexer
  (:require [clojure.string :as str]
            [slingshot.slingshot :refer [throw+]]))
  ; (:import (dk.brics.automaton RegExp Automaton RunAutomaton State)))

(defrecord Token [type text line char])

(defn- get-next-token [patmap linestr linenum prevcharnum]
  "Given patterns and lexer state, get next token. Patterns must begin
   with '^' to match start of line."
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
                 :msg (str "Failed to parse token: " trimmed-line)})) 
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
            (println "Just scanned token" tok-type match-text)
            {:token (Token. tok-type match-text linenum cur-char)
             :next-lex-state {:linenum next-linenum
                              :linestr rest-line
                              :charnum next-char}})
          ; No match; try another pattern
          (recur (rest patterns)))))))

(defn- get-next-line [lines]
  (let [first-line (first lines)
        rest-lines (rest lines)]
    (if (str/blank? first-line)
      (if (empty? rest-lines) 
        [nil nil]
        ; If current line is blank, skip to next line
        (recur rest-lines))
      ; Trim because trailing whitespace will mess with lexer and it
      ; doesn't matter in C anyways
      [(str/trimr first-line) rest-lines])))

(defn- lex
  ([patterns lines] 
   (let [[first-line rest-lines] (get-next-line lines)] 
     (lex patterns [] first-line rest-lines 1 1)))
  ([patterns tokens cur-line rest-lines linenum charnum]
   (let [res (get-next-token patterns cur-line linenum charnum)
         new-token (:token res)
         new-tokens (conj tokens new-token)
         new-state (:next-lex-state res)
         line-exhausted? (empty? (:linestr new-state))
         [new-cur-line new-rest-lines] (if line-exhausted? 
                                         (get-next-line rest-lines) 
                                         [(:linestr new-state) rest-lines])
         new-linenum (:linenum new-state)
         new-charnum (if line-exhausted? 1 (:charnum new-state))]
     ; (println (:linestr new-state) new-cur-line new-rest-lines)
     ; This line is empty and there's no next line (at EOF)
     (if (and (empty? new-cur-line) (empty? new-rest-lines))
       new-tokens
       (recur patterns new-tokens new-cur-line new-rest-lines 
              new-linenum new-charnum)))))

(def eof-token {:type :end-of-file})
    
(defn make-lexer [patterns]
  "Creates a [string] -> [token] lexer from patterns"
  (fn lexer [string]
    (conj (lex patterns (str/split-lines string)) eof-token)))
