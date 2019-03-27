(ns cljtinyc.llparser
  "Implementation of the LL(1) parser."
  (:require [clojure.set :as s]
            [clojure.pprint :as pp]
            [slingshot.slingshot :refer [throw+]]
            [cljtinyc.node :as n]))

;; =========
;; Utilities
;; =========

(defn ^:private remove-epsilon [s]
  (s/difference s #{:epsilon}))

(defn ^:private get-all-symbols [grammar]
  (set (flatten grammar)))

;; ==============================
;; FIRST, FOLLOW, and FIRST+ sets
;; ==============================

(defn ^:private get-first-for-beta [beta fsets]
  (if (empty? beta)
    ; If there are no more elements in beta, then every element of beta might
    ; derive epsilon, so beta as a whole might also derive epsilon. So we put
    ; epsilon in FIRST(beta)
    #{:epsilon}
    (let [beta1 (first beta)
          b1-fset (beta1 fsets)]
      ; If FIRST(b1) contains epsilon, then the correct derivation of beta1
      ; might be epsilon, so first of beta is the union of FIRST(b1) \ epsilon
      ; and FIRST(b2). epsilon is NOT part of FIRST(b1) because if 
      ; A -> beta1 beta2 and beta1 is epsilon, then FIRST(A) is now
      ; FIRST(b2), and unless beta2 also derives epsilon, FIRST(A) does NOT
      ; contain epsilon. 
      (if (contains? b1-fset :epsilon)
        (s/union (remove-epsilon b1-fset)
                 (get-first-for-beta (rest beta) fsets))
        b1-fset))))

(defn ^:private first-contrib-of-productions [productions first-sets]
  (loop [prods productions
         fsets first-sets]
    (if (empty? prods)
      ; If there are no more productions, then we're done
      fsets
      ; From each production A -> beta1 beta2 ..., we are calculating the new
      ; FIRST(A) by the algorithm described in get-first-for-beta
      (let [prod (first prods)
            nt (first prod)
            nt-fset (nt fsets)
            beta (rest prod)
            beta-fset (get-first-for-beta beta,fsets)
            ; The new FIRST(A) is the union of the previous FIRST(A) and 
            ; FIRST(beta)
            new-nt-fset (s/union beta-fset nt-fset)]
        (recur (rest prods)
               ; The new FIRST(A) is FIRST(beta)
               (assoc fsets nt new-nt-fset))))))

(defn ^:private get-first-sets 
  [grammar first-sets]
  (let [new-fsets (first-contrib-of-productions grammar first-sets)]
    ; Keep going through the algorithm until we reach a fixed point
    (if (= new-fsets first-sets)
      new-fsets
      (recur grammar new-fsets))))

(defn ^:private setup-first-sets [terminals non-terminals]
  ; The initial fsets for nonterminals are empty and only the terminal
  ; for terminals
  (let [nt-fsets (reduce (fn [map nt] (assoc map nt #{}))
                         {} non-terminals)
        t-fsets (reduce (fn [map term] (assoc map term #{term}))
                        {} terminals)]
    ; (println "Terminals: " terminals)
    ; (println "Non-Terminals: " non-terminals)
    (merge nt-fsets t-fsets)))


(defn ^:private production-follow-contrib 
  "Calculates the production's contribution to the overall FOLLOW set."
  [production first-sets follow-sets non-terminals]
  (let [nt (first production)
        rhs (rest production)]
    (loop [; RHS symbols are processed in REVERSE because each symbol needs
           ; to affect the FOLLOW set of the one before itself
           beta (reverse rhs)
           folsets follow-sets
           ; The trailing suffix of the entire production is the FOLLOW of the
           ; non-terminal on the left deriving the entire expression, since
           ; that is what follows the rule
           trailer (nt follow-sets)]
      ; (println beta folsets trailer)
      (if (empty? beta)
        folsets
        (let [beta-last (first beta)]
          (if (contains? non-terminals beta-last)
            ; If beta-last is a non-terminal, then its follow is
            ; is whatever trailing suffices that we have accumulated
            (let [prev-beta-last-folset (beta-last folsets)
                  ; Add the trailer to the FOLLOW of beta-last
                  new-beta-last-folset (s/union trailer prev-beta-last-folset)
                  first-beta-last (beta-last first-sets)
                  new-trailer 
                  ; If beta-last can derive epsilon, then the trailer is 
                  ; whatever has accumulated PLUS FIRST(beta-last), since
                  ; in the case that beta-last derives epsilon, the symbol
                  ; before it will have whatever is trailing follow it
                  (if (contains? first-beta-last :epsilon)
                    (s/union trailer (remove-epsilon first-beta-last))               
                    first-beta-last)]
              (recur (rest beta)
                     (assoc folsets beta-last new-beta-last-folset)
                     new-trailer))
            ; If beta-last is a terminal symbol, then the trailing suffix for
            ; anything before beta-last is just be the FIRST of itself, which
            ; should be the set containing only itself
            (recur (rest beta) folsets (beta-last first-sets))))))))

(defn ^:private process-prods-for-follow
  [productions first-sets follow-sets non-terminals]
  (if (empty? productions)
    follow-sets
    (recur (rest productions)
           first-sets
           (production-follow-contrib 
              (first productions) first-sets follow-sets non-terminals)
           non-terminals)))

(defn ^:private get-follow-sets 
  [grammar first-sets follow-sets non-terminals]
  (let [new-folsets (process-prods-for-follow
                       grammar first-sets follow-sets non-terminals)]
    ; Same as FIRST calculation: keep going until we reach a fixed point
    ; where the productions don't contribute anything new
    (if (= follow-sets new-folsets)
      new-folsets
      (recur grammar first-sets new-folsets non-terminals))))

(defn ^:private setup-follow-sets [non-terminals]
  (reduce (fn [map nt] (assoc map nt #{}))
          {} non-terminals))

(defn ^:private get-augfirst-for-prod [production first-sets follow-sets]
  (let [nt (first production)
        beta (rest production)
        ; If we have the production A -> beta1 beta2, then beta
        ; is just beta = beta1 beta2 ...
        first-beta (get-first-for-beta beta first-sets)]
    (if (contains? first-beta :epsilon)
      ; For a production A -> beta, if the production can 
      ; derive epsilon, then the augmented first is
      ; the union of FIRST(beta) and FOLLOW(A)
      (s/union first-beta (follow-sets nt))
      first-beta)))

; (defn ^:private get-production-augfirsts
;   [first-sets follow-sets augfirst-sets production]
;   (assoc augfirst-sets production 
;          (augfirst-for-prod production first-sets follow-sets)))

;; ====================================
;; Building the LL(1) table and parsing
;; ====================================

(def ^:private example-parser-state 
  {:table {:nonterminal1 {:terminal1 #() :terminal2 #()}}
   :terminals #{:terminal1 :terminal2}
   :stack '()
   :tokens []
   :output []
   :errors []})

(defn ^:private parse
  "Given the previous state of the parser, output the state after the stack
   is consumed"
  [{:keys [table stack tokens output errors] :as state}]
  ; (println "Current stack:" stack)
  (if (empty? stack)
    state
    (let [focus (first stack)
          lookahead-token (first tokens)
          lookahead (:type lookahead-token)
          ; _ (println "Focus" focus "Lookahead" lookahead)
          table-entry (-> table focus lookahead)
          ; _ (println "Table entry" table-entry)
          new-state (table-entry state)]
      (recur new-state))))

(defn ^:private entry-for-production [production]
  (fn [parser-state]
    (let [{:keys [table stack tokens output errors]} parser-state
          lookahead-type (:type (first tokens))
          focus (first stack)
          nt (first production)
          rhs (rest production)
          ; Filter out epsilons because they don't (or are not supposed to)
          ; match anything
          rhs-processed (filter #(not= % :epsilon) rhs)
          prepared-state (assoc parser-state 
                           :stack rhs-processed
                           :output [])
          new-state (parse prepared-state)
          ; The whole point of this algorithm is here: the parse should give
          ; us a list of all the children in the :output, which should be 
          ; reduced into the child of the non-terminal we are currently
          ; expanding on the parse tree
          parsed-node (n/make-node nt (:output new-state))]
      (assoc new-state 
        ; We restore the stack to what it was before with this production
        ; popped off
        :stack (rest stack)
        :output (conj (:output parser-state) parsed-node)))))

(defn ^:private entry-for-terminal [{:keys [stack tokens output] :as state}]
  (if (= (first stack) (:type (first tokens)))
    ; The top of stack matches the next token; pop it off and add
    ; to the derivation / parse tree
    (assoc state :stack (rest stack)
                 :tokens (rest tokens)
                 :output (conj output (n/make-node (first tokens))))
    ; The top of stack does not match the input, we just detected an 
    ; unexpected token.
    ; TODO: better error handling
    (throw+ {:type ::unexpected-token
             :expected (peek stack)
             :found (first tokens)})))

(defn ^:private add-entry-for-prodrule [augfirst-sets table prodrule]
  "Since each production of the form A -> b1 b2 b3... will only affect the
   A row of the table, we take the previous entry in the table for A and then
   assoc into in all the new entries we need to add for every element in the 
   FIRST+ of the production"
  (let [augfirst (augfirst-sets prodrule)
        entry (entry-for-production prodrule)
        nt (first prodrule)
        ; _ (println "Nonterminal:" nt)
        red-fn (fn [nt-entry symbol] (assoc nt-entry symbol entry))
        prev-nt-entry (table nt)
        ; _ (println "Previous:" prev-nt-entry)
        new-nt-entry (reduce red-fn prev-nt-entry augfirst)]
        ; _ (println "New entry:" new-nt-entry)]
    (assoc table nt new-nt-entry)))

(defn ^:private add-entry-for-terminal [table terminal]
  ; Only accept the terminal. All other spots are left empty to signify
  ; an error to the parser. Custom error handling can be added to other
  ; terminals if neccessary
  (assoc table terminal {terminal entry-for-terminal}))

;; ===============
;; Public API
;; ===============

(defn get-grammar-meta [grammar]
  (let [all-symbols (get-all-symbols grammar)
        non-terminals (set (map first grammar))
        terminals (s/difference all-symbols non-terminals)
        modified-terminals (into terminals #{:epsilon :end-of-file})]
    {:grammar grammar
     :all-symbols all-symbols
     :non-terminals non-terminals
     :terminals modified-terminals}))

(defn calculate-first-sets
  "Calculates the FIRST sets for each non-terminal in the grammar.
   Grammar should be specified as a list of productions of the form 
   A -> B1 B2 B3 with no alternations. Alternations should be 
   listed as seperate productions.

   For example: instead of A -> B | C, use A -> B; A -> C

   Any symbol on the left hand side of a production is assumed to be a
   non-terminal, and those not on the right are assumed to be a terminal.
   The first production in the grammar is assumed to be the start."
  [grammar]
  (let [{:keys [all-symbols non-terminals terminals]} (get-grammar-meta grammar)
        fsets (setup-first-sets terminals non-terminals)]
    (get-first-sets grammar fsets)))

(defn calculate-follow-sets 
  "Calculate the FOLLOW set for a list of productions. Use the same format as
   described in FRIST. Requires a correct FIRST."
  [grammar first-sets]
  (let [{:keys [all-symbols non-terminals]} (get-grammar-meta grammar)
        empty-folsets (setup-follow-sets non-terminals)
        start-symbol (first (first grammar))
        follow-sets (assoc empty-folsets start-symbol #{:end-of-file})]
    (get-follow-sets grammar first-sets follow-sets non-terminals)))

(defn calculate-augfirst-sets
  "Calculate the FIRST+ sets for each of the production rules."
  [grammar-meta first-sets follow-sets]
  (let [{:keys [grammar]} grammar-meta
        map-fn (fn [prod] 
                 [prod (get-augfirst-for-prod prod first-sets follow-sets)])
        mapped (mapv map-fn grammar)]
    (into {} mapped)))

(defn build-parsing-table
  "Builds the parsing table"
  [grammar-meta augfirst-sets]
  (let [{:keys [grammar non-terminals terminals]} grammar-meta
        partial-table (reduce add-entry-for-terminal {} terminals)
        ; _ (pp/pprint partial-table)
        add-prodrule-fn (partial add-entry-for-prodrule augfirst-sets)
        full-table (reduce add-prodrule-fn partial-table grammar)]
    full-table))

(defn get-parser
  "Get the token stream -> parse tree parser given a grammar."
  [grammar start-sym]
  (fn [tokens]
    (let [grammar-meta (get-grammar-meta grammar)
          firsts (calculate-first-sets grammar)
          follows (calculate-follow-sets grammar firsts)
          augfirst (calculate-augfirst-sets grammar-meta firsts follows)
          table (build-parsing-table grammar-meta augfirst)
          init-state {:table table
                      :terminals (:terminals grammar-meta)
                      :stack (list start-sym)
                      :tokens tokens
                      :output []
                      :errors []}
          final-state (parse init-state)]
      (:output final-state))))

; For REPL use

(def test-grammar
  [[:E  :T :Ep]
   [:Ep :plus :T :Ep]
   [:Ep :epsilon]
   [:T  :F :Tp]
   [:Tp :star :F :Tp]
   [:Tp :epsilon]
   [:F  :lparen :E :rparen]
   [:F  :id]])

(def test-token-stream
  [{:type :id} {:type :star} {:type :id} {:type :end-of-file}])

(def test-meta (get-grammar-meta test-grammar))
(def test-first (calculate-first-sets test-grammar))
(def test-follow (calculate-follow-sets test-grammar test-first))
(def afs (calculate-augfirst-sets test-meta test-first test-follow))
(def lltable (build-parsing-table test-meta afs))
(def init-state {:table lltable
                 :stack '(:E)
                 :terminals (:terminals test-meta)
                 :tokens test-token-stream
                 :output []
                 :errors []})

(def test-parser (get-parser test-grammar :E))