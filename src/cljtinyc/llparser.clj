(ns cljtinyc.llparser
  "Implementation of the LL(1) parser."
  (:require [clojure.set :as s]
            [clojure.pprint :as pp]))

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
        (s/union (s/difference b1-fset #{:epsilon})
                 (get-first-for-beta (rest beta) fsets))
        b1-fset))))

(defn ^:private process-productions [productions first-sets]
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
  (loop [fsets first-sets]
    (let [new-fsets (process-productions grammar fsets)]
      ; Keep going through the algorithm until we reach a fixed point
      (if (= new-fsets fsets)
        new-fsets
        (recur new-fsets)))))

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

(defn ^:private get-all-symbols [grammar])
  (set (flatten grammar))

(defn calculate-first-sets
  "Calculates the FIRST sets for each non-terminal in the grammar.
   Grammar should be specified as a list of productions of the form 
   A -> B1 B2 B3 with no alternations. Alternations should be 
   listed as seperate productions.

   Any symbol on the left hand side of a production is assumed to be a
   non-terminal, and those not on the right are assumed to be a terminal."
  [grammar]
  (let [all-symbols (get-all-symbols grammar)
        non-terminals (set (map first grammar))
        terminals (s/difference all-symbols non-terminals)
        modified-terminals (into terminals #{:epsilon :end-of-file})
        fsets (setup-first-sets terminals non-terminals)]
    (get-first-sets grammar fsets)))

(defn calculate-follow-set
  "Calculates the FOLLOW sets for each non-terminal in the grammar. The 
   grammar should be specified the same as for `calculate-first-set`"
  [grammar])
