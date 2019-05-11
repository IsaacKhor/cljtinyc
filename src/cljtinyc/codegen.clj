(ns cljtinyc.codegen
  (:require [clojure.string :as s]))

(defmulti vc-to-str first)
(defmethod vc-to-str :ir/reg [[_ num]] (str "$" num))
(defmethod vc-to-str :ir/label [[_ name]] name)
(defmethod vc-to-str :default [arg] 
  (AssertionError. (str arg " not supported")))

(defn comp-to-str [comp]
  (if (vector? comp)
    (vc-to-str comp)
    comp))

(defn stringify-args [args]
  (if (>= 1 (count args))
    (comp-to-str (first args))
    (str (comp-to-str (first args)) ", " (stringify-args (rest args)))))

(defn gen-instruction [op args] 
  (if (nil? op) (throw (Exception. (str [op args] "not supported"))))
  (str op " " (stringify-args args)))

(defn table-entry-to-asm [[name entry]]
  ; For now, pretend like everything is an int. Replace with actual size
  ; based on information from symbol table later
  (str name ": " ".word" " " 0))

(def op-to-inst-table
  {:ir/nop            "nop"
   :ir/system-call    "syscall"

   ; Memory
   :ir/load           "lw"
   :ir/load-immediate "li"
   :ir/store          "sw"
   :ir/move-from-hi   "mfhi"
   :ir/move-from-lo   "mflo"
   :ir/move-reg       "move"
   
   ; Branches
   :ir/branch-eq    "beq"
   :ir/branch-eq0   "beqz"
   :ir/branch-neq   "bne"
   :ir/branch-neq0  "bnez"
   :ir/unconditional-jump "j"

   ; Comparisons
   :ir/op-equals        "seq"
   :ir/op-noteq         "sne"
   :ir/op-lessthaneq    "sle"
   :ir/op-lessthan      "slt" 
   :ir/op-greaterthaneq "sge"
   :ir/op-greaterthan   "sgt"

   ; Logic
   :ir/op-logical-or    "or"
   :ir/op-logical-and   "and"

   ; Arithmetic
   :ir/op-addition        "add"
   :ir/op-subtraction     "sub"
   :ir/op-multiplication  "mul"
   :ir/op-division        "div"})

(defmulti lir-to-asmline first)
(defmethod lir-to-asmline :ir/label [[_ name]] (str name ":"))
(defmethod lir-to-asmline :default [[op & args]]
  (let [inst (op-to-inst-table op)]
    (when (nil? inst) 
      (throw (Exception. (str "No instruction for" op))))
    (gen-instruction inst args)))

(defn asm-generate [symbol-table linear-ir]
  (let [data-segment (map table-entry-to-asm (seq symbol-table))
        code-segment (map lir-to-asmline linear-ir)]
    (str ".data\n" 
         (s/join "\n" data-segment) 
         "\n.text\n" 
         (s/join "\n" code-segment)
         "\n")))
