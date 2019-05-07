(ns cljtinyc.iriset)

; Utility functions for ir gen
(defn reg [num]                    [:ir/reg-tmp num])
(defn regarg [num]                 [:ir/reg-arg num])
(defn regval [num]                 [:ir/reg-val num])
(defn regmv [to from]              [:ir/move-reg to from])
(defn load [register value]        [:ir/load register value])
(defn store [identifier register]  [:ir/store identifier register])
(defn label [name]                 [:ir/label name])
(defn jump [label]                 [:ir/unconditional-jump label])
(defn branch-neq0 [register target-label] 
  [:ir/branch-neq register :ir/register-zero target-label])
(defn syscall [] [:ir/system-call])
