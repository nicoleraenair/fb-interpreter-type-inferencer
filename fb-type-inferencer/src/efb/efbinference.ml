open Batteries;;
open Fb;;

open Efbtype;;
open Fbast;;

(** This exception is raised if a type cannot be inferred for an expression. *)
exception TypeError;;
exception NotClosed;;


(** This mutable variable stores a counter which will generate fresh type
    variable names. *)
let _next_tvar_num = ref 1;;

(** This helper function produces a fresh type variable name each time it is
    called.  You are not required to use this function (and are permitted to
    change it if you wish), but you'll need some way to create fresh type
    variables during type inference. *)
let fresh_tvar () : efbtype =
  let idx = !_next_tvar_num in
  _next_tvar_num := idx + 1;
  TVar("a" ^ string_of_int idx)
;;

module Equation = struct
  type t = efbtype * efbtype
  let compare = Stdlib.compare
end
module EqnSet = Set.Make(Equation);;

module VarKey = struct
  type t = string
  let compare = Stdlib.compare
end;;
module VarMap = Map.Make(VarKey);;

(*performs the initial inference of type equations for the expression.*)
let rec infer_eqn (gamma : efbtype VarMap.t) (e : expr) : (efbtype * EqnSet.t) =
  match e with
  | Int _ -> (TInt, EqnSet.empty)
  | Bool _ -> (TBool, EqnSet.empty)
  | Not exp -> (
    let (t, e) = infer_eqn gamma exp in
    (TBool, (EqnSet.add (t, TBool) e))
  )
  | And (exp1, exp2) -> (
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (TBool, (EqnSet.add (t2, TBool) (EqnSet.add (t1, TBool) (EqnSet.union e1 e2))))
  )
  | Or (exp1, exp2) -> (
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (TBool, (EqnSet.add (t2, TBool) (EqnSet.add (t1, TBool) (EqnSet.union e1 e2))))
  )
  | Plus (exp1, exp2) -> (
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (TInt, (EqnSet.add (t2, TInt) (EqnSet.add (t1, TInt) (EqnSet.union e1 e2))))
  )
  | Minus (exp1, exp2) -> (
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (TInt, (EqnSet.add (t2, TInt) (EqnSet.add (t1, TInt) (EqnSet.union e1 e2))))
  )
  | Equal (exp1, exp2) -> (
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (TBool, (EqnSet.add (t2, TInt) (EqnSet.add (t1, TInt) (EqnSet.union e1 e2))))
  )
  | If (exp1, exp2, exp3) -> (
    let alpha = fresh_tvar () in
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    let (t3, e3) = infer_eqn gamma exp3 in
    (alpha, (EqnSet.add (t3, alpha) (EqnSet.add (t2, alpha) (EqnSet.add (t1, TBool) (EqnSet.union (EqnSet.union e1 e2) e3)))))
  )
  | Function (arg, exp) -> (
    let alpha = fresh_tvar () in
    let newgamma = VarMap.add arg alpha gamma in
    let (t, e) = infer_eqn newgamma exp in
    (TArrow (alpha, t), e)
  )
  | Appl (exp1, exp2) -> (
    let alpha = fresh_tvar () in
    let (t1, e1) = infer_eqn gamma exp1 in
    let (t2, e2) = infer_eqn gamma exp2 in
    (alpha, (EqnSet.add (t1, TArrow (t2, alpha)) (EqnSet.union e1 e2)))
  )
  | Var v -> (
    match VarMap.find_opt v gamma with
    | None -> raise NotClosed
    | Some (b) -> (b, EqnSet.empty) )
  | Let (x, e1, e2) -> infer_eqn gamma (Appl (Function (x, e2), e1))
  | _ -> raise TypeError
;;

let rec symmetry (l: Equation.t list) : EqnSet.t = 
  match l with
  | [] -> EqnSet.empty
  | (left, right) :: tail -> EqnSet.add (right, left) (symmetry tail)
;;

let rec subtransitivity (head : Equation.t) (tail: Equation.t list) : EqnSet.t =
  let (t1,t2) = head in
    match tail with
    | [] -> EqnSet.empty
    | (t3,t4) :: subtail -> EqnSet.add
        (if t1 = t3 then
          (t2, t4)
        else if t1 = t4 then
          (t2, t3)
        else if t2 = t3 then
          (t1, t4)
        else if t2 = t4 then
          (t1, t3)
        else 
          (t1,t2))
        (subtransitivity head subtail)
;;

let rec transitivity (l: Equation.t list) : EqnSet.t = 
  match l with
  | [] -> EqnSet.empty
  | head :: tail -> EqnSet.union (subtransitivity head tail) (transitivity tail)
;;

let rec decomposition (l: Equation.t list) : EqnSet.t = 
  match l with
  | [] -> EqnSet.empty
  | head :: tail -> (match head with
    | (TArrow(t1, t2),TArrow(t3, t4)) -> EqnSet.add (t2, t4) (EqnSet.add (t1, t3) (decomposition tail))
    | _ -> decomposition tail
  )
;;

(*perform a deductive logical closure of that set of type equations + check that set of type equations for consistency*)
let rec closure (eqnset: EqnSet.t) : EqnSet.t =
  let eqnList = EqnSet.to_list eqnset in
  let symm = symmetry eqnList in
  let trans = transitivity eqnList in
  let decomp = decomposition eqnList in
  let newEqns = EqnSet.union (EqnSet.union (EqnSet.union eqnset symm) trans) decomp in
    if EqnSet.cardinal eqnset = EqnSet.cardinal newEqns then
      eqnset
    else
      closure newEqns
;;

let is_valid (eqn : efbtype * efbtype) : bool = 
  match eqn with
  | (TInt, TBool) -> false
  | (TInt, TArrow (_,_)) -> false
  | (TBool, TArrow (_,_)) -> false
  | _ -> true
;;

let rec is_consistent (l : Equation.t list) : bool = 
  match l with
  | [] -> true
  | h :: t -> (is_valid h) && (is_consistent t)
;;

let rec lexleast (l : efbtype list) : string = 
  match l with
  | TVar v :: [] -> v
  | TVar v :: t -> if v < (lexleast t) then v else lexleast t
  | _ -> raise TypeError
;;

let rec filter_type (l: Equation.t list) (t0 : efbtype) : efbtype list = 
  match l with
  | [] -> []
  | (t1, t2) :: t -> if t1 = t0 then t2 :: (filter_type t t0) else (filter_type t t0)
;;

(*perform type substitution on the resulting constrained type to provide a displayable type to the user*)
let rec type_sub (t0 : efbtype) (eqnset: EqnSet.t) : efbtype = 
  match t0 with
  | TArrow (t1,t2) -> TArrow (type_sub t1 eqnset, type_sub t2 eqnset)
  | TBool | TInt -> t0
  | TVar v -> (
    let types = filter_type (EqnSet.to_list eqnset) t0 in
    let concretes = List.filter (fun x -> 
        match x with
        | TInt | TBool | TArrow (_,_) -> true
        | _ -> false
      ) types
    in
    match concretes with
    | [] -> 
      (let vars = List.filter (fun x -> 
        match x with
        | TVar _ -> true
        | _ -> false
      ) types
      in
      if vars = [] then TVar v else
        (if (lexleast vars) < v then TVar (lexleast vars) else TVar v))
    | h :: _ -> type_sub h eqnset
  )
;;

(** This function infers the type of a particular expression. *)
let infer_type (e : expr) : efbtype =
  let (t, eqnset) = (infer_eqn VarMap.empty e) in 
  let c = closure eqnset in 
    if is_consistent (EqnSet.to_list c) then
      type_sub t c
    else
      raise TypeError
;;