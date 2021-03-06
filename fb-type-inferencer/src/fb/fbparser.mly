%{

open Fbast;;

%}

/*
 * Tokens
 */

%token AND
%token <bool> BOOL
%token ELSE
%token EOEX
%token EQUAL
%token FUNCTION
%token GOESTO
%token <string> IDENT
%token IN
%token IF
%token <int> INT
%token LET
%token LPAREN
%token MINUS
%token NOT
%token OR
%token PLUS
%token RPAREN
%token REC
%token THEN
%token EOF

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%right prec_let                         /* Let Rec f x = ... In ... */
%right prec_fun                         /* function declaration */
%right prec_if                          /* If ... Then ... Else */
%right OR                               /* Or */
%right AND                              /* And */
%left EQUAL                             /* = */
%left PLUS MINUS                        /* + - */
%right NOT                              /* not e */

/*
 * The entry point.
 */
%start main
%type <(Fbast.expr * bool) option> main
%start expr_eof
%type <Fbast.expr> expr_eof

%%

main:
  | expr EOEX
      { Some ($1,true) }
  | expr EOF
      { Some ($1,false) }
  | EOF
      { None }
;

expr_eof:
  | expr EOF
      { $1 }
;

expr:
  | appl_expr
      { $1 }
  | expr PLUS expr
      { Plus($1, $3) }
  | expr MINUS expr
      { Minus($1, $3) }
  | expr AND expr
      { And($1, $3) }
  | expr OR expr
      { Or($1, $3) }
  | NOT expr
      { Not $2 }
  | expr EQUAL expr
      { Equal($1, $3) }
  | FUNCTION ident_decl GOESTO expr %prec prec_fun
      { Function($2, $4) }
  | LET ident_decl EQUAL expr IN expr %prec prec_let
      { Let($2, $4, $6) }
  | LET REC ident_decl ident_decl EQUAL expr IN expr %prec prec_let
      { LetRec($3, $4, $6, $8) }
  | IF expr THEN expr ELSE expr %prec prec_if
      { If($2, $4, $6) }
;

appl_expr:
    negatable_expr
      { $1 }
  | appl_expr simple_expr
      { Appl($1,$2) }
;

negatable_expr:
    MINUS INT
      { Int (-$2) }
  | simple_expr
      { $1 }
;

simple_expr:
    INT
      { Int $1 }
  | BOOL
      { Bool $1 }
  | ident_usage
      { $1 }
  | LPAREN expr RPAREN
      { $2 }
;

ident_usage:
    ident_decl
      { Var $1 }
;

ident_decl:
    IDENT
      { $1 }
;

%%
