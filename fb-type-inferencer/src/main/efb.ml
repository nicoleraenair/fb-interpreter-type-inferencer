(**
   This is the F♭ interpreter toploop.
*)

open Efb;;
open Fb;;

open Efbtype;;

(* ****** Utility functions ****** *)

let string_of_exception (exc : exn) : string =
  Printf.sprintf "Exception: %s\n" (Printexc.to_string exc)
;;

(* ****** Parsing for command-line arguments ****** *)

type configuration =
  {
    conf_filename : string option;
    (** If a filename is provided on the command line, it appears here. *)
  }
;;

let parse_command_line () : configuration =
  let filename = ref None in
  Arg.parse
    []
    (fun filename_arg ->
       filename := Some filename_arg
    )
    ("Usage: ./fb [filename]\n")
  ;
  {
    conf_filename = !filename
  }
;;

(* ****** Processing expressions ****** *)

let handle_next_expression ?eval_decoration:(eval_decoration=true) lexbuf =
  (* Parse the expression *)
  let maybe_expr, stop =
    try
      match Fbparser.main Fblexer.token lexbuf with
      | Some(expr,continuing) -> (Some(expr), not continuing)
      | None -> (None, true)
    with
    | exc ->
      print_endline (Printf.sprintf "Parse error: %s"
                       (string_of_exception exc));
      (None, false)
  in
  begin
    match maybe_expr with
    | Some expr ->
      begin
        let type_safe =
          try
            let typ = Efbinference.infer_type expr in
            print_endline (": " ^ string_of_efbtype typ);
            true
          with
          | exc ->
            print_endline (Printf.sprintf "Type error: %s"
                             (string_of_exception exc));
            false
        in
        if type_safe then begin
          try
            let result = Fbinterp.eval expr in
            if eval_decoration then
              print_endline (Printf.sprintf "==> %s"
                               (Fbpp.pretty_print result))
            else
              print_string (Fbpp.pretty_print result)
          with
          | exc ->
            print_endline (Printf.sprintf "Evaluation error: %s"
                             (string_of_exception exc))
        end
      end
    | None -> ()
  end;
  not stop
;;

let do_toploop () =
  print_endline "Welcome to F♭!";
  print_endline "Please enter expressions terminated by two semicolons.";
  print_endline "";
  let lexbuf = Lexing.from_channel stdin in
  let rec toploop () =
    if handle_next_expression lexbuf then
      toploop ()
    else
      ()
  in
  toploop ();
  print_endline "Goodbye!";;
;;

let run_file filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  ignore (handle_next_expression ~eval_decoration:false lexbuf);
  close_in file
;;

(* ****** Entry point ****** *)

let main () =
  let config = parse_command_line () in
  match config.conf_filename with
  | None ->
    do_toploop ()
  | Some filename ->
    run_file filename
;;

main ();;
