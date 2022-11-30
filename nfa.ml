open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    List.fold_left (fun acc (x, y, z) -> if List.mem x qs then (if y = s && (List.mem z acc = false) then z :: acc else acc) else acc) [] nfa.delta

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
  let rec e_closure_aux (nfa: ('q,'s) nfa_t) (qs: 'q list) acc = 
    match qs with 
    | [] -> acc
    | h :: t -> if (List.mem h acc = false) then e_closure_aux nfa ((move nfa [h] None) @ t) (h :: acc) 
                  else e_closure_aux nfa t acc
  in e_closure_aux nfa qs []

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec accept_aux (nfa: ('q,char) nfa_t) (lst: char list) (qs: 'q list) = 
    match lst with    
    | [] -> List.fold_left (fun acc x -> if List.mem x nfa.fs then true else acc) false qs
    | h :: t -> accept_aux nfa t (e_closure nfa (move nfa qs (Some h)))
  in accept_aux nfa (explode s) (e_closure nfa [nfa.q0])  

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc x -> e_closure nfa (move nfa qs (Some x)) :: acc) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun acc x -> (qs, Some x, e_closure nfa (move nfa qs (Some x))) :: acc) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun acc x -> if List.mem x nfa.fs then [qs] else acc) [] qs

let rec convert_qs (nfa: ('q,'s) nfa_t) (work: 'q list list) (visited: 'q list list) : 'q list list = 
  match work with
  | [] -> visited
  | h :: t -> if List.mem h visited then convert_qs nfa t visited 
                else convert_qs nfa (t @ new_states nfa h) (h :: visited)
                
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let dfa = 
  { qs = convert_qs nfa [e_closure nfa [nfa.q0]] []
  ; sigma = nfa.sigma
  ; delta = List.fold_left (fun acc x -> (new_trans nfa x) @ acc) [] (convert_qs nfa [e_closure nfa [nfa.q0]] [])
  ; q0 = e_closure nfa [nfa.q0]
  ; fs = List.fold_left (fun acc x -> (new_finals nfa x) @ acc) [] (convert_qs nfa [e_closure nfa [nfa.q0]] []) } in dfa