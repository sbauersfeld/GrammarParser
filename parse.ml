type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
;;

(*this searches an old grammar and returns a list of the 
right hand sides that match the input nonterminal*)
let rec produce_rhs rules target = match rules with
  | [] -> []
  | head::tail -> 
    if (fst head) = target (*If a match is found, return the right hand side*)
      then (snd head)::(produce_rhs tail target) 
    else produce_rhs tail target
;;

(*This uses the above function to create the style of grammar 
specified in this assignment*)
let convert_grammar g = 
  (fst g, produce_rhs (snd g))
;;

(*parse tree is similar but recursive function must also return the rules expanded, acceptor is empty list*)

(*This function recurses through the left hand side of the tree 
until a leaf node is reached, at which point it returns the leaf
and concatenates it to the result of recursing through the remaining
tree with the left branch removed. Recreating the node of (x, tail)
is used to recurse through the tree with the left branch removed.*)
let rec parse_tree_leaves tree = match tree with
  | Node (x, front::tail) -> (parse_tree_leaves front) @ (parse_tree_leaves (Node (x, tail)))
  | Leaf t -> [t]
  | _ -> []
;;

let is_accepted = function
  | Some _ -> true
  | None -> false
;;

(*This function takes a list of parse subtrees as input as well as a head
non terminal along with the right hand side of the non terminal. It returns
a list of parse subtrees such that the head non terminal is the parent of the
node with a number of children equal to the number of right hand side symbols.*)
let rec create_tree head children subtree result = match children, subtree with
  | [], _ -> Node(head, result)::subtree
  | _, [] -> []
  | next::rest, left_branch::right_side -> create_tree head rest right_side (result @ [left_branch])
;;

(*These mutually recusive functions do the majprity of the work for matching and creating parse trees.
Iterate rules until a terminal node reached, if the terminal matches head of input fragments, return 
fragments with the head removed. If the recursive function receives return value of with value false, 
There was not a match. Continue until top level function is completely matched. Then apply the remaining 
fragment list to the acceptor. Repeat on all possible terms until acceptor returns true.*)
let rec nested_matcher nodes rules frag acceptor = match nodes, frag with 
  | [], _ -> frag, true, []
  | _, [] -> [], false, []
  | T front::rest, head::tail -> 
      if front = head then (*If the terminal leaf matches the first fragment, continue with that prefix removed*)
        let (frag_res, bool_res, tree_res) = nested_matcher rest rules tail acceptor in
          frag_res, bool_res, (Leaf front)::tree_res (*Prepend the leaf to the parse subtree list*)
      else [], false, []
  (*If the next item is a node, expand the possibe right hand sides*)
  | N front::rest, head::tail -> apply_options rest rules frag acceptor front (rules front)
              
(*This function expands all of the possible right hand sides for a node to continue searching the tree*)
and apply_options nodes rules frag acceptor head = function
 | [] -> frag, false, [] (*If no right hand sides could be matched, this branch is invalid*)
 | front::rest -> 
    (*Prepend the right hand side terminals to the existing nodes to perform DFS type search on the tree*)
    let (frag_res, bool_res, tree_res) = nested_matcher (front @ nodes) rules frag acceptor in 
      if bool_res && is_accepted (acceptor frag_res) 
        then frag_res, true, create_tree head front tree_res []
      else apply_options nodes rules frag acceptor head rest (*try the next right hand side*)
;;                                                

(*This is the upper level matcher that returns the result of the acceptor 
applied to the suffix produced by the matcher functions above*)
let my_matcher g acceptor frag = match g with
  | start, rules -> let (frag_res, bool_res, tree_res) = 
      apply_options [] rules frag acceptor start (rules start) in (*build the tree to search and begin matching*)
        if bool_res && is_accepted (acceptor frag_res) 
          then acceptor frag_res (*return what the matcher returned*)
        else None
;;

(*This simply returns the matcher function for the grammar g*)
let make_matcher g = my_matcher g
;;

(*This is the acceptor used for creating a parse tree. A parse tree is only valid if the
suffix of the matched fragment is empty*)
let my_acceptor = function
  | _::_ -> None
  | x -> Some x
;;

(*The parser function is very similar to the matcher function, except that it returns
the parse tree if the acceptor succeeds*)
let my_parser g frag = match g with
| start, rules -> let (frag_res, bool_res, tree_res) = 
    apply_options [] rules frag my_acceptor start (rules start) in
      if bool_res && is_accepted (my_acceptor frag_res) 
        then Some (List.hd tree_res) (*Return the constructed parse tree*)
      else None
;;

(*This returns the parser function for the grammar g*)
let make_parser g = my_parser g
;;