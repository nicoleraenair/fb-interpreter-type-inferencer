type ident = string
[@@deriving eq, ord, show]
;;

type expr =
  | Int of int
  | Bool of bool
  | Var of ident
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Equal of expr * expr
  | If of expr * expr * expr
  | Function of ident * expr
  | Appl of expr * expr
  | Let of ident * expr * expr
  | LetRec of ident * ident * expr * expr
[@@deriving eq, ord, show]
;;
