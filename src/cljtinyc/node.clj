(ns cljtinyc.node)

; Node is, well, a node in a tree. Note that the tree is ORDERED since
; parse trees by their very nature have to be.
; 
; Fields:
;  - symbol: The symbol, terminal or non-terminal, of the node. If this is a
;            terminal, then the children field should be empty.
;  - childrer: Self-explanatory. Empty when `symbol` is a terminal
;  - properties: Map of properties of the node
(defrecord Node [symbol children properties])

(defn make-node
  "Make a node, optionally passing in children and properties."
  ([symbol]                     (Node. symbol [] {}))
  ([symbol children]            (Node. symbol children {}))
  ([symbol children properties] (Node. symbol children properties)))

(defn add-child
  "Returns a new `Node` with the child added to the children list of the
   parent. Expects the child to be a `Node`."
  [parent child]
  (Node. (:symbol parent)
         (conj (:children parent) child)
         (:properties parent)))

(defn replace-children
  "Returns a new `Node` with the child replaced."
  [node new-children]
  (Node. (:symbol node)
         new-children
         (:properties node)))