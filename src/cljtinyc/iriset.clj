(ns cljtinyc.iriset)

; Utility functions for ir gen

; Registers
; Temporary registers start at 8, args at 4, and val at 2
(def regzero [:ir/reg 0])
(defn reg [num]                    [:ir/reg (+ 8 num)])
(defn regraw [num]                 [:ir/reg num])
(defn regarg [num]                 [:ir/reg (+ 4 num)])
(defn regval [num]                 [:ir/reg (+ 2 num)])

(defn regmv [to from]              [:ir/move-reg to from])
(defn load [register value]        [:ir2/load register value])
(defn store [to from]              [:ir/store from to])

; Labels
(defn label [name]                 [:ir/label name])
(defn label-with-num [name num]    [:ir/label (str name "_" num)])

; Branching
(defn jump [label]                 [:ir/unconditional-jump label])
(defn branch-neq0 [register target-label] 
  [:ir/branch-neq0 register target-label])
(defn branch-eq0 [register target-label]
  [:ir/branch-eq0 register target-label])

(defn syscall [] [:ir/system-call])
