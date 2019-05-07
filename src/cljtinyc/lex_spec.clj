(ns cljtinyc.lex-spec
  "Lexical definition for C. Taken mostly from here: 
   https://github.com/daveho/catparty/blob/master/src/catparty/clexer.cljc"
  (:require [cljtinyc.lexer :as lex]))

(def c-keywords
  ["auto" "bool" "break" "case" "char" "const" "continue" "default" "double" "do"
   "else" "enum" "extern" "float" "for" "goto" "if" "int" "long" "register"
   "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef"
   "union" "unsigned" "void" "volatile" "while"])

(def keyword-patterns 
  (mapv 
    (fn [kw] [(keyword (str "kw_" kw))
              (re-pattern (str "^" kw))])
    c-keywords))

(def identifier-pattern [:identifier #"^[a-zA-Z_][a-zA-Z0-9_]*"])

(def operator-patterns
  [[:op_eq #"^=="]
   [:op_assign #"^="]
   [:op_lshift_assign #"^<<="]
   [:op_lshift #"^<<"]
   [:op_lte #"^<="]
   [:op_lt #"^<"]
   [:op_rshift_assign #"^>>="]
   [:op_rshift #"^>>"]
   [:op_gte #"^>="]
   [:op_gt #"^>"]
   [:op_ne #"^!="]
   [:op_not #"^!"]
   [:op_or #"^\|\|"]
   [:op_bit_or_assign #"^\|="]
   [:op_bit_or #"^\|"]
   [:op_and #"^&&"]
   [:op_bit_and_assign #"^&="]
   [:op_amp #"^&"] ; note that this means bitwise-and and address-of
   [:op_bit_xor_assign #"^\^="]
   [:op_bit_xor #"^\^"]
   [:op_bit_compl #"^~"]
   [:op_plus_assign #"^\+="]
   [:op_inc #"^\+\+"]
   [:op_plus #"^\+"]
   [:op_minus_assign #"^-="]
   [:op_dec #"^--"]
   [:op_arrow #"^->"]
   [:op_minus #"^-"]
   [:op_mul_assign #"^\*="]
   [:op_star #"^\*"] ; note that this means multiplication and pointer/dereference
   [:op_div_assign #"^/="]
   [:op_div #"^/"]
   [:op_mod_assign #"^%="]
   [:op_mod #"^%"]])

(def punctuation-patterns
  [[:lparen #"^\("]
   [:rparen #"^\)"]
   [:lbracket #"^\["]
   [:rbracket #"^\]"]
   [:lbrace #"^\{"]
   [:rbrace #"^\}"]
   [:colon #"^:"]
   [:semicolon #"^;"]
   [:comma #"^,"]
   [:ques #"^\?"]
   [:ellipsis #"^\.\.\."]
   [:dot #"^\."]])

(def literal-patterns
  [[:literal_fp #"^[0-9]+\.[0-9]*([Ee](\+|-)?[0-9+])?[fFlL]?"]
   [:fp_literal #"^\.[0-9]+([Ee](\+|-)?[0-9+])?[fFlL]?"]
   [:hex_literal #"^0[Xx][0-9A-Fa-f]+(u|U)?(ll|LL)"]
   [:hex_literal #"^0[Xx][0-9A-Fa-f]+(u|U)?(l|L)?"]
   [:dec_literal #"^[0-9]+(u|U)?(ll|LL)"]
   [:dec_literal #"^[0-9]+(u|U)?(l|L)?"]
   [:char_literal #"^'(\\([ntvbrfa\\?'\"]|[0-7][0-7][0-7]|[0-9A-Fa-f][0-9A-Fa-f])|[^\\'])'"]
   [:string_literal #"^\"(\.|[^\"])*\""]])

(def lex-grammar
  (concat keyword-patterns 
          [identifier-pattern] 
          ; Must be before punctuation to scan ".3" correctly
          literal-patterns
          operator-patterns 
          punctuation-patterns)) 

(def c-lexer (lex/make-lexer lex-grammar))