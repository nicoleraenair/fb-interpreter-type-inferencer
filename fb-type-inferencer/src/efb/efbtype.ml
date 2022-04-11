type efbtype =
  | TInt
  | TBool
  | TArrow of efbtype * efbtype (* function type: input -> output *)
  | TVar of string (* type variables identified by name *)
[@@deriving eq, ord, show];;

let rec string_of_efbtype (t : efbtype) =
  match t with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow(t1,t2) ->
    begin
      let t1s =
        match t1 with
        | TArrow _ -> "(" ^ string_of_efbtype t1 ^ ")"
        | _ -> string_of_efbtype t1
      in
      t1s ^ " -> " ^ string_of_efbtype t2
    end
  | TVar(s) -> "'" ^ s
;;
