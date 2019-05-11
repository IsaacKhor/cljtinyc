(ns cljtinyc.irgen
  (:require [cljtinyc.iriset :as ir]))

(defrecord TableEntry [type context])
(defrecord IrNode [node children])

(defn get-children [node] (:children node))
(defn get-node-val [node] (:node node))

(def symbol-table* (atom {}))
(def label-number* (atom 1))
(def max-registers 16)
(def register-tmp (ir/reg 3))

(defn ^:private node-rename [node]
  (condp = node

    ; Stuff that gets processed in 2nd pass
    :Program         :ir1/statement-list
    :Block           :ir1/block
    :Assign_expr     :ir1/assign
    :While_expr      :ir1/while-loop
    :If_expr         :ir1/if-statement
    :Statement_list  :ir1/statement-list
    :BinaryOp        :ir1/expression
    :UnaryOp         :ir1/expression
    :Declarations    :ir1/declaration-list
    :Declaration     :ir1/declaration
    :Function_call   :ir1/syscall
    :Identifier_list :ir1/identifier-list

    ; Proccess in 3rd pass
    "%"              :ir2/op-modulo
    "!"              :ir2/op-logical-not

    ; CG instructions
    "=="             :ir/op-equals
    "!="             :ir/op-noteq
    "<="             :ir/op-lessthaneq
    "<"              :ir/op-lessthan
    ">="             :ir/op-greaterthaneq
    ">"              :ir/op-greaterthan
    "||"             :ir/op-logical-or
    "&&"             :ir/op-logical-and
    "+"              :ir/op-addition
    "-"              :ir/op-subtraction
    "*"              :ir/op-multiplication
    "/"              :ir/op-division
    node))

(defn ^:private to-irnode [ast-node]
  (if (not (coll? ast-node))
    ; Rename here since we're already walking the tree
    (IrNode. (node-rename ast-node) nil)
    (IrNode. (node-rename (first ast-node))
             (mapv to-irnode (rest ast-node)))))

(defn ^:private process-type
  [type-str]
  (cond
    (= type-str "int")  (TableEntry. :type/int32 :ctx/global)
    (= type-str "bool") (TableEntry. :type/int8  :ctx/global)
    :else               (TableEntry. :type/void  :ctx/global)))

(defn ^:private decl-list-to-symtable
  "Generate symbol table form AST declaration list"
  [symtable declarations]
  (if (empty? declarations)
    symtable 
    (let [decl (first declarations)
          ; AST nodes are [:Declaration "type" [:Identifier_list blah]]
          [type-str idens] (get-children decl)
          type (process-type type-str)
          ; AST nodes are [:Identifier_list foo bar var1 var2]
          vars (get-children idens)
          new-st (reduce (fn [table var] 
                           (assoc table (get-node-val var) type)) 
                         symtable vars)]
      (recur new-st (rest declarations)))))

(defn ir-pass1 
  "First IR pass: change from hiccup to records and rename"
  [ast]
  (to-irnode ast))

;;; =======================
;;; ===== 2nd IR Pass =====
;;; =======================

; Ershov numbers

(defn get-ernum [node] (:ershov node))
(defn add-ernum [node num] (assoc node :ershov num))

(defn calculate-ernum [left-num right-num]
  ; Algorithm is as such:
  ; - All leaf gets 1 (implemented in base case below)
  ; - Node with 1 child gets number of child
  ; - Node with 1 child gets greater of the two children if different or 
  ;   1 + children if they are the same
  (if (nil? right-num) ; 1 child scenario
    left-num
    (if (= left-num right-num)
      (inc left-num)
      (max left-num right-num))))

(defn ershov-calc [node]
  (if (empty? (get-children node))
    (add-ernum node 1)
    (let [children (get-children node)
          ; Do post order traversal so we visit children before parents
          mapped-children (mapv ershov-calc children)
          left-num (get-ernum (second mapped-children))
          ; May be nil when there's only 1 child
          right-num (get-ernum (first (rest (rest mapped-children))))
          ; Make sure we're using children with metadata applied
          new-node (IrNode. (get-node-val node) mapped-children)
          node-num (calculate-ernum left-num right-num)]
      (add-ernum new-node node-num))))

; IR generation for expressions

(declare expr-codegen)

(defn ecd-empty [ann-node base]
  ; No children means its a terminal node, which can be an operator
  ; or a numerical literal. Either way, we won't decide whether 
  ; we'll be loading a variable or literal this pass; that's decided
  ; at the code generation phase
  [(ir/load (ir/reg base) (get-node-val ann-node))])

(defn ecd-unary-case [ann-node base]
  ; We recursively generate code for child using base b, and their
  ; result will appear in R_b+k-1
  (let [c (get-children ann-node)
        child-node (second c)
        operator (get-node-val (first c))
        child-code (expr-codegen (nth c 1) base)
        child-ernumber (get-ernum child-node)
        out-reg (ir/reg (+ base child-ernumber -1))
        ; LIR for R = OP R
        gen-code [operator out-reg out-reg]]
    (conj child-code gen-code)))

(defn ecd-child-equal [ann-node base]
  ; Numbers are equal, generate right child in base b+1 with result 
  ; in b+k-1 and left child in b+k-2, then merge
  (let [[opnode lnode rnode] (get-children ann-node)
        operator (get-node-val opnode)
        ernum (get-ernum ann-node) ; We use the ernum of the parent
        right-code (expr-codegen rnode (+ 1 base))
        right-reg (ir/reg (+ base ernum -1))
        left-code (expr-codegen lnode base)
        left-reg (ir/reg (+ base ernum -2))
        merged-code (into right-code left-code)
        ; LIR for right = OP left right
        gen-code [operator right-reg left-reg right-reg]]
    (conj merged-code gen-code)))

(defn ecd-child-unequal [ann-node base]
  ; Numbers are not equal, generate child with larger label first
  ; with base b, result in b+k-1, then the smaller child with
  ; base b, result in b+m-1 (m in label of smaller child), then merge
  (let [[opnode lnode rnode] (get-children ann-node)
        operator (get-node-val opnode)
        lnum (get-ernum lnode)
        rnum (get-ernum rnode)
        [bignode smallnode] (if (> rnum lnum) [rnode lnode] [lnode rnode])
        bignum (max rnum lnum)
        bigcode (expr-codegen bignode base)
        bigreg (ir/reg (+ base bignum -1))
        smallnum (min rnum lnum)
        smallcode (expr-codegen smallnode base)
        smallreg (ir/reg (+ base smallnum -1))
        merged (into bigcode smallcode)
        gen-code
        (if (> rnum lnum)
          ; LIR for bigger = OP smaller bigger, if right is bigger
          [operator bigreg smallreg bigreg]
          [operator bigreg bigreg smallreg])]
    (conj merged gen-code)))

(defn temp-var [num] (str "compiler_temp_" num))

(defn ecd-exceed-register [ann-node base]
  (let [[opnode lnode rnode] (get-children ann-node)
        operator (get-node-val opnode)
        lnum (get-ernum lnode)
        rnum (get-ernum rnode)
        [bignode smallnode] (if (>= rnum lnum) [rnode lnode] [lnode rnode])

        ; Process larger node
        bignum (max rnum lnum)
        bigcode (expr-codegen bignode base)
        bigreg (ir/reg max-registers)

        ; Process smaller node
        smallnum (min rnum lnum)
        smallbase (if (> smallnum max-registers) 1 (- max-registers smallnum))

        ; Tmp variable load/store
        ; Add tmp variable to symbol table
        tmp-var (temp-var bignum)
        _ (swap! symbol-table* assoc tmp-var (process-type "int"))
        reg-r-1 (ir/reg (- max-registers 1))]
    `[~@bigcode
      (ir/store tmp-var bigreg) ; Store to memory temporarily
      ; Code for the node with lower ernumber
      ~@(expr-codegen smallnode smallbase)
      (ir/load reg-r-1 tmp-var)
      (if (>= rnum lnum)
          ; LIR for bigger = OP smaller bigger, if right is bigger
          [operator bigreg bigreg reg-r-1]
          [operator bigreg reg-r-1 bigreg])]))

(defn expr-codegen [ann-node base]
  (let [c (get-children ann-node)
        type (get-node-val ann-node)]
    (cond
      (empty? c)      (ecd-empty ann-node base)
      (= 2 (count c)) (ecd-unary-case ann-node base)
      :else
      (let [left-num (get-ernum (nth c 1))
            right-num (get-ernum (nth c 2))]
            ; _ (println "node" ann-node)
            ; _ (println "base" base "lnum" left-num "rnum" right-num)]
        (if (or (> left-num max-registers) (> right-num max-registers))
          (ecd-exceed-register ann-node base)
          (if (= right-num left-num) 
            (ecd-child-equal ann-node base)
            (ecd-child-unequal ann-node base)))))))

(defn get-result-register [expr-ir]
  ; Instructions are [Op R1 R2 R3], we want R1
  ; peek gives us the last element for vectors
  (let [instruction (peek expr-ir)
        [op r1] instruction]
    r1))

(defmulti genir get-node-val)

(defmethod genir :ir1/expression [node]
  (expr-codegen (ershov-calc node) 1))

(defmethod genir :ir1/statement-list [node]
  ; Children of a ir/statement-list node is just a list of statements
  (vec (apply concat (map genir (get-children node)))))

(defmethod genir :ir1/assign [node]
  ; If we have foo = expr, we generate the code for the expr
  ; then add on a store instruction
  (let [[iden-node expr] (get-children node)
        lin-ir (genir expr)
        result-reg (get-result-register lin-ir)
        identifier (get-node-val iden-node)
        gen-code (ir/store identifier result-reg)]
    (conj lin-ir gen-code)))

(defmethod genir :ir1/if-statement [node]
  (let [[expr left] (get-children node) 
        expr-ir (genir expr)
        expr-result (get-result-register expr-ir)
        label-num (swap! label-number* inc)
        label-alt (ir/label-with-num "if_alt" label-num)
        label-end (ir/label-with-num "if_end" label-num)
        left-ir (genir left)]
    (if (= 2 (count (get-children node)))
      ; Has no else branch. We want something like:
      ; [expression-ir...
      ;  if not expr-result goto end
      ;  left-ir...
      ;  [label end]]
      `[~@expr-ir
        ~(ir/branch-eq0 expr-result label-end)
        ~@left-ir
        ~label-end]
      ; Else branch is present, we want IR like this:
      ; [expression-ir...
      ;  if not expr-result goto alt
      ;  left-ir
      ;  goto end
      ;  alt:
      ;  right-ir
      ;  end:]
      `[~@expr-ir
        ~(ir/branch-eq0 expr-result label-alt)
        ~@left-ir
        ~(ir/jump label-end)
        ~label-alt
        ~@(genir (nth (get-children node) 2))
        ~label-end])))

(defmethod genir :ir1/while-loop [node]
  (let [[expr block] (get-children node)
        expr-ir (genir expr)
        expr-result (get-result-register expr-ir)
        lnum (swap! label-number* inc)
        lstart (ir/label-with-num "while_start" lnum)
        lend (ir/label-with-num "while_end" lnum)]
    `[~lstart
      ~@expr-ir
      ~(ir/branch-eq0 expr-result lend)
      ~@(genir block)
      ~(ir/jump lstart)
      ~lend]))

(defmethod genir :ir1/block [node]
  (let [[declarations code] (get-children node)
        sm (decl-list-to-symtable @symbol-table* (get-children declarations))
        ; Add declarations to the symbol table
        _ (swap! symbol-table* merge sm)]
    (genir code)))

(defmethod genir :ir1/syscall [node]
  (let [; Assume its always a printf for a number for the sake of simplicity
        [_ expr] (get-children node)
        expr-ir (genir expr)
        expr-res (get-result-register expr-ir)]
    (into expr-ir 
          [(ir/regmv (ir/regarg 0) expr-res)
           ; 1 for printing an int
           (ir/load (ir/regval 0) 1)
           (ir/syscall)])))

(defmethod genir :default [node]
  [(ir/load register-tmp (get-node-val node))])

(defn ir-pass2
  "Second IR pass: generate low-level linear IR"
  [ir1]
  ; Add main label and exit syscall
  `[~(ir/label "main")
    ~@(genir ir1)
    ~(ir/load (ir/regval 0) 10)
    ~(ir/syscall)])

;;; =======================
;;; ===== 3nd IR Pass =====
;;; =======================

(defmulti rewrite-inst first)

(defmethod rewrite-inst :ir2/load [[_ to from]]
  (if (string? from)
    ; Loading from a register v from a value embedded in source
    [[:ir/load to from]]
    [[:ir/load-immediate to from]]))

(defmethod rewrite-inst :ir2/op-modulo [[_ store rega regb]]
  ; On division, quotient is written to LO and remainder to HI
  [[:ir/op-division rega regb]
   ; Insert no-ops due to potential pipeline hazards
   [:ir/nop]
   [:ir/nop]
   [:ir/move-from-hi store]])

(defmethod rewrite-inst :ir2/op-logical-not [[_ reg]]
  (let [lnum (swap! label-number* inc)
        ltrue (ir/label-with-num "lnot" lnum)
        lend (ir/label-with-num "lnot_end" lnum)]
    ; Implement truth table manually because we don't want
    ; the bitwise negation
    [[:ir/branch-eq reg ir/regzero ltrue]
     [:ir/load-immediate reg 0]
     (ir/jump lend)
     ltrue
     [:ir/load-immediate reg 1]
     lend]))

(defmethod rewrite-inst :default [instruction]
  [instruction])

(defn ir-pass3
  "Third IR pass: rewrite instructions to match MIPS instruction set"
  [ir2]
  (vec (mapcat rewrite-inst ir2)))
