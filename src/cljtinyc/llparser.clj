(ns cljtinyc.llparser
  "Implementation of the LL(1) parser."
  (:require [clojure.set :as s]
            [clojure.pprint :as pp]))

(defn ^:private remove-epsilon [s]
  (s/difference s #{:epsilon}))

(defn ^:private get-all-symbols [grammar]
  (set (flatten grammar)))

(defn ^:private get-grammar-meta [grammar]
  (let [all-symbols (get-all-symbols grammar)
        non-terminals (set (map first grammar))
        terminals (s/difference all-symbols non-terminals)
        modified-terminals (into terminals #{:epsilon :end-of-file})]
    {:all-symbols all-symbols
     :non-terminals non-terminals
     :terminals modified-terminals}))

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

(defn calculate-first-sets
  "Calculates the FIRST sets for each non-terminal in the grammar.
   Grammar should be specified as a list of productions of the form 
   A -> B1 B2 B3 with no alternations. Alternations should be 
   listed as seperate productions.

   For example: instead of A -> B | C, use A -> B; A -> C

   Any symbol on the left hand side of a production is assumed to be a
   non-terminal, and those not on the right are assumed to be a terminal."
  [grammar]
  (let [{:keys [all-symbols non-terminals terminals]} (get-grammar-meta grammar)
        fsets (setup-first-sets terminals non-terminals)]
    (get-first-sets grammar fsets)))

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
            ; If beta-last is a non-terminal, then the FOLLOW of the symbol
            ; before beta-last is whatever trailing suffices that we have 
            ; accumulated
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

(defn calculate-follow-sets 
  "Calculate the FOLLOW set for a list of productions. Use the same format as
   described in FRIST. Requires a corrent FIRST."
  [grammar first-sets]
  (let [{:keys [all-symbols non-terminals]} (get-grammar-meta grammar)
        empty-folsets (setup-follow-sets non-terminals)
        start-symbol (first (first grammar))
        follow-sets (assoc empty-folsets start-symbol #{:end-of-file})]
    (get-follow-sets grammar first-sets follow-sets non-terminals)))