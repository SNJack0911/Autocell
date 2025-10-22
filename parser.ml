type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | LPAREN
  | RPAREN
  | ID of (
# 66 "parser.mly"
        string
# 34 "parser.ml"
)
  | INT of (
# 67 "parser.mly"
       int
# 39 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param ws	Sequence of (expression, conditions) terminated
				by (expression, NO_COND).
	@return		Built statement. *)
let rec make_when f ws =
	match ws with
	| [(e, NO_COND)]	->	f e
	| (e, c)::t			-> IF_THEN(c, f e, make_when f t)
	| _ -> failwith "whens list not ended by (expression, NO_COND)."

# 68 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* DOT_DOT *);
  265 (* DOT *);
  266 (* NEG *);
  267 (* ADD *);
  268 (* SUB *);
  269 (* MUL *);
  270 (* DIV *);
  271 (* MOD *);
  272 (* IF *);
  273 (* THEN *);
  274 (* ELSE *);
  275 (* ELSEIF *);
  276 (* LT *);
  277 (* GT *);
  278 (* LE *);
  279 (* GE *);
  280 (* EQ *);
  281 (* NE *);
  282 (* LPAREN *);
  283 (* RPAREN *);
    0|]

let yytransl_block = [|
  284 (* ID *);
  285 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\007\000\007\000\006\000\006\000\006\000\006\000\008\000\008\000\
\008\000\008\000\008\000\008\000\009\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\000\000\005\000\008\000\006\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\001\000\001\000\001\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\007\000\000\000\000\000\
\002\000\000\000\005\000\000\000\001\000\000\000\000\000\000\000\
\008\000\000\000\006\000\000\000\000\000\000\000\000\000\023\000\
\022\000\000\000\024\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\029\000\030\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\
\012\000\007\000\000\000\000\000\000\000\011\000\007\000\000\000"

let yydgoto = "\002\000\
\004\000\009\000\018\000\010\000\011\000\025\000\072\000\034\000\
\035\000\036\000"

let yysindex = "\007\000\
\240\254\000\000\014\255\000\000\017\255\239\254\020\255\011\255\
\030\255\032\255\000\000\007\255\010\255\000\000\019\255\040\255\
\000\000\096\000\000\000\034\255\000\000\035\255\254\254\036\255\
\000\000\061\255\000\000\064\255\254\254\254\254\254\254\000\000\
\000\000\053\255\000\000\092\255\254\254\254\254\043\255\042\255\
\042\255\039\255\000\000\254\254\254\254\254\254\254\254\254\254\
\254\254\254\254\254\254\254\254\254\254\254\254\031\255\031\255\
\078\255\000\000\000\255\042\255\042\255\000\000\000\000\000\000\
\031\255\031\255\031\255\031\255\031\255\031\255\000\000\003\255\
\000\000\000\000\254\254\001\255\054\255\000\000\000\000\000\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\084\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\097\000\102\000\
\000\000\000\000\012\255\049\000\073\000\000\000\000\000\000\000\
\070\255\071\255\072\255\073\255\074\255\075\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\255"

let yygindex = "\000\000\
\000\000\000\000\215\255\000\000\078\000\000\000\000\000\019\000\
\238\255\030\000"

let yytablesize = 386
let yytable = "\026\000\
\025\000\059\000\078\000\022\000\073\000\022\000\022\000\001\000\
\029\000\030\000\007\000\008\000\003\000\009\000\005\000\023\000\
\023\000\010\000\013\000\006\000\074\000\075\000\012\000\031\000\
\026\000\032\000\033\000\024\000\024\000\009\000\009\000\014\000\
\076\000\010\000\010\000\016\000\015\000\080\000\017\000\037\000\
\026\000\044\000\045\000\046\000\047\000\048\000\007\000\020\000\
\027\000\044\000\045\000\046\000\047\000\048\000\046\000\047\000\
\048\000\026\000\040\000\041\000\042\000\026\000\027\000\028\000\
\038\000\058\000\055\000\056\000\039\000\043\000\079\000\057\000\
\028\000\060\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\003\000\015\000\016\000\
\017\000\018\000\019\000\020\000\019\000\077\000\000\000\021\000\
\014\000\000\000\000\000\000\000\000\000\013\000\044\000\045\000\
\046\000\047\000\048\000\000\000\000\000\000\000\000\000\049\000\
\050\000\051\000\052\000\053\000\054\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\025\000\025\000\000\000\000\000\000\000\
\025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\026\000\025\000\025\000\000\000\026\000\000\000\
\000\000\000\000\000\000\026\000\026\000\000\000\000\000\000\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\027\000\026\000\026\000\000\000\027\000\000\000\
\000\000\000\000\000\000\027\000\027\000\000\000\000\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\028\000\027\000\027\000\000\000\028\000\000\000\
\000\000\000\000\000\000\028\000\028\000\000\000\000\000\000\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\014\000\028\000\028\000\022\000\014\000\013\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\023\000\
\014\000\000\000\014\000\014\000\000\000\013\000\000\000\013\000\
\013\000\000\000\000\000\024\000\014\000\000\000\000\000\000\000\
\000\000\013\000"

let yycheck = "\018\000\
\000\000\043\000\002\001\006\001\002\001\006\001\006\001\001\000\
\011\001\012\001\028\001\029\001\029\001\002\001\001\001\016\001\
\016\001\002\001\008\001\003\001\018\001\019\001\003\001\026\001\
\000\000\028\001\029\001\028\001\028\001\018\001\019\001\002\001\
\074\000\018\001\019\001\029\001\005\001\079\000\029\001\004\001\
\059\000\011\001\012\001\013\001\014\001\015\001\028\001\008\001\
\000\000\011\001\012\001\013\001\014\001\015\001\013\001\014\001\
\015\001\076\000\029\000\030\000\031\000\080\000\029\001\029\001\
\004\001\027\001\037\000\038\000\005\001\017\001\017\001\029\001\
\000\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\007\001\002\001\017\001\017\001\
\017\001\017\001\017\001\017\001\015\000\075\000\255\255\000\000\
\000\000\255\255\255\255\255\255\255\255\000\000\011\001\012\001\
\013\001\014\001\015\001\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\027\001\028\001\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\027\001\028\001\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\027\001\028\001\255\255\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\027\001\028\001\006\001\006\001\002\001\
\255\255\255\255\255\255\006\001\255\255\255\255\255\255\016\001\
\016\001\255\255\018\001\019\001\255\255\016\001\255\255\018\001\
\019\001\255\255\255\255\028\001\028\001\255\255\255\255\255\255\
\255\255\028\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  NEG\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  MOD\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSEIF\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  EQ\000\
  NE\000\
  LPAREN\000\
  RPAREN\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 78 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 314 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 325 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 91 "parser.mly"
  ( set_fields _1 )
# 332 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 96 "parser.mly"
  ( [_1] )
# 339 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 98 "parser.mly"
  (_3 :: _1 )
# 347 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 103 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 359 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
  ( NOP )
# 365 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 113 "parser.mly"
  ( SEQ(_1, _2) )
# 373 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
        ( [] )
# 379 "parser.ml"
               : 'elseif_branches))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'elseif_branches) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 119 "parser.mly"
        ( (_3, _5) :: _1 )
# 388 "parser.ml"
               : 'elseif_branches))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'elseif_branches) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 124 "parser.mly"
        ( 
          (* fold right to nest IF_THENs *)
          let rec build_if cond then_branch elseifs else_branch =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, else_branch)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest else_branch)
          in
          build_if _2 _4 _5 _7
        )
# 406 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elseif_branches) in
    Obj.repr(
# 134 "parser.mly"
        (
          let rec build_if cond then_branch elseifs =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, NOP)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest)
          in
          build_if _2 _4 _5
        )
# 422 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 143 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 434 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 149 "parser.mly"
  (
			(* declare the variable (assign a register) and build SET_VAR *)
            let r = declare_var _1 in
            SET_VAR (r, _3)
		)
# 446 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 157 "parser.mly"
  ( COMP(COMP_LT,_1, _3) )
# 454 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 159 "parser.mly"
  ( COMP(COMP_GT,_1, _3) )
# 462 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 161 "parser.mly"
  ( COMP(COMP_LE,_1, _3) )
# 470 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 163 "parser.mly"
  ( COMP(COMP_GE,_1, _3) )
# 478 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 165 "parser.mly"
  ( COMP(COMP_EQ,_1, _3) )
# 486 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 167 "parser.mly"
  ( COMP(COMP_NE,_1, _3) )
# 494 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 172 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "y out of range";
			(_2, _4)
		)
# 506 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 181 "parser.mly"
        ( CST(_1) )
# 513 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "parser.mly"
        ( let r = get_var _1 in
          if r = -1 then error (sprintf "undeclared variable %s" _1)
          else VAR r )
# 522 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 187 "parser.mly"
        ( CELL(0,fst _1, snd _1) )
# 529 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 189 "parser.mly"
        ( _2 )
# 536 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 191 "parser.mly"
        ( NEG(_2) )
# 543 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 193 "parser.mly"
        ( BINOP(OP_ADD, _1, _3) )
# 551 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 195 "parser.mly"
        ( BINOP(OP_SUB, _1, _3) )
# 559 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 197 "parser.mly"
        ( BINOP(OP_MUL, _1, _3) )
# 567 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 199 "parser.mly"
        ( BINOP(OP_DIV, _1, _3) )
# 575 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 201 "parser.mly"
        ( BINOP(OP_MOD, _1, _3) )
# 583 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 203 "parser.mly"
        ( _2 )
# 590 "parser.ml"
               : 'expression))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
