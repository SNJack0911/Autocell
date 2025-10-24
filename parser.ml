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
  | OR
  | AND
  | NOT
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | LPAREN
  | RPAREN
  | ID of (
# 69 "parser.mly"
        string
# 37 "parser.ml"
)
  | INT of (
# 70 "parser.mly"
       int
# 42 "parser.ml"
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

# 71 "parser.ml"
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
  276 (* OR *);
  277 (* AND *);
  278 (* NOT *);
  279 (* LT *);
  280 (* GT *);
  281 (* LE *);
  282 (* GE *);
  283 (* EQ *);
  284 (* NE *);
  285 (* LPAREN *);
  286 (* RPAREN *);
    0|]

let yytransl_block = [|
  287 (* ID *);
  288 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\007\000\007\000\006\000\006\000\006\000\006\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\000\000\005\000\008\000\006\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\007\000\000\000\000\000\
\002\000\000\000\005\000\000\000\001\000\000\000\000\000\000\000\
\008\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\027\000\026\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\036\000\000\000\000\000\016\000\000\000\000\000\033\000\
\034\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\000\000\012\000\007\000\000\000\000\000\000\000\011\000\
\007\000\000\000"

let yydgoto = "\002\000\
\004\000\009\000\018\000\010\000\011\000\025\000\082\000\035\000\
\036\000\037\000"

let yysindex = "\009\000\
\240\254\000\000\044\255\000\000\045\255\008\255\055\255\058\255\
\069\255\070\255\000\000\015\255\051\255\000\000\053\255\088\255\
\000\000\098\000\000\000\067\255\000\000\081\255\002\255\098\255\
\000\000\140\255\000\000\124\255\006\255\006\255\002\255\002\255\
\000\000\000\000\042\255\000\000\112\255\006\255\006\255\118\255\
\006\255\145\255\145\255\000\000\127\255\092\255\000\000\002\255\
\002\255\006\255\006\255\006\255\006\255\006\255\006\255\006\255\
\006\255\006\255\006\255\006\255\066\255\066\255\144\255\039\255\
\000\000\000\000\255\254\133\255\000\000\145\255\145\255\000\000\
\000\000\000\000\066\255\066\255\066\255\066\255\066\255\066\255\
\000\000\004\255\000\000\000\000\002\255\005\255\132\255\000\000\
\000\000\255\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\154\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\109\000\114\000\000\000\000\000\
\000\000\000\000\007\255\012\255\000\000\055\000\082\000\000\000\
\000\000\000\000\040\255\080\255\091\255\111\255\113\255\125\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255"

let yygindex = "\000\000\
\000\000\000\000\213\255\000\000\146\000\000\000\000\000\227\255\
\238\255\035\000"

let yytablesize = 401
let yytable = "\026\000\
\029\000\044\000\045\000\067\000\022\000\083\000\088\000\022\000\
\009\000\001\000\022\000\022\000\029\000\030\000\023\000\003\000\
\029\000\030\000\068\000\069\000\023\000\084\000\085\000\031\000\
\009\000\009\000\010\000\030\000\017\000\024\000\032\000\017\000\
\033\000\034\000\041\000\024\000\033\000\034\000\007\000\008\000\
\086\000\017\000\010\000\010\000\005\000\090\000\016\000\006\000\
\026\000\050\000\051\000\052\000\053\000\054\000\031\000\087\000\
\018\000\012\000\047\000\018\000\018\000\048\000\049\000\042\000\
\043\000\013\000\046\000\026\000\066\000\018\000\014\000\026\000\
\061\000\062\000\015\000\064\000\050\000\051\000\052\000\053\000\
\054\000\032\000\017\000\007\000\070\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\020\000\
\019\000\021\000\027\000\019\000\019\000\038\000\050\000\051\000\
\052\000\053\000\054\000\020\000\014\000\019\000\020\000\020\000\
\028\000\013\000\055\000\056\000\057\000\058\000\059\000\060\000\
\020\000\066\000\050\000\051\000\052\000\053\000\054\000\021\000\
\040\000\022\000\021\000\021\000\022\000\022\000\055\000\056\000\
\057\000\058\000\059\000\060\000\021\000\023\000\022\000\039\000\
\023\000\023\000\048\000\049\000\089\000\063\000\081\000\048\000\
\049\000\049\000\023\000\003\000\065\000\052\000\053\000\054\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\029\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\029\000\029\000\000\000\000\000\000\000\
\029\000\029\000\029\000\029\000\029\000\029\000\000\000\029\000\
\029\000\029\000\029\000\029\000\029\000\030\000\029\000\029\000\
\000\000\030\000\000\000\000\000\000\000\000\000\030\000\030\000\
\000\000\000\000\000\000\030\000\030\000\030\000\030\000\030\000\
\030\000\000\000\030\000\030\000\030\000\030\000\030\000\030\000\
\031\000\030\000\030\000\000\000\031\000\000\000\000\000\000\000\
\000\000\031\000\031\000\000\000\000\000\000\000\031\000\031\000\
\031\000\031\000\031\000\031\000\000\000\031\000\031\000\031\000\
\031\000\031\000\031\000\032\000\031\000\031\000\000\000\032\000\
\000\000\000\000\000\000\000\000\032\000\032\000\000\000\000\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\022\000\
\032\000\032\000\032\000\032\000\032\000\032\000\014\000\032\000\
\032\000\023\000\014\000\013\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\014\000\000\000\014\000\014\000\
\024\000\013\000\000\000\013\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\013\000"

let yycheck = "\018\000\
\000\000\031\000\032\000\047\000\006\001\002\001\002\001\006\001\
\002\001\001\000\006\001\006\001\011\001\012\001\016\001\032\001\
\011\001\012\001\048\000\049\000\016\001\018\001\019\001\022\001\
\018\001\019\001\002\001\000\000\017\001\031\001\029\001\020\001\
\031\001\032\001\029\001\031\001\031\001\032\001\031\001\032\001\
\084\000\030\001\018\001\019\001\001\001\089\000\032\001\003\001\
\067\000\011\001\012\001\013\001\014\001\015\001\000\000\085\000\
\017\001\003\001\017\001\020\001\021\001\020\001\021\001\029\000\
\030\000\008\001\032\000\086\000\030\001\030\001\002\001\090\000\
\038\000\039\000\005\001\041\000\011\001\012\001\013\001\014\001\
\015\001\000\000\032\001\031\001\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\058\000\059\000\060\000\008\001\
\017\001\000\000\032\001\020\001\021\001\004\001\011\001\012\001\
\013\001\014\001\015\001\017\001\000\000\030\001\020\001\021\001\
\032\001\000\000\023\001\024\001\025\001\026\001\027\001\028\001\
\030\001\030\001\011\001\012\001\013\001\014\001\015\001\017\001\
\005\001\017\001\020\001\021\001\020\001\021\001\023\001\024\001\
\025\001\026\001\027\001\028\001\030\001\017\001\030\001\004\001\
\020\001\021\001\020\001\021\001\017\001\032\001\007\001\020\001\
\021\001\021\001\030\001\002\001\030\001\013\001\014\001\015\001\
\015\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\016\001\017\001\018\001\019\001\020\001\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\002\001\030\001\031\001\
\255\255\006\001\255\255\255\255\255\255\255\255\011\001\012\001\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\023\001\024\001\025\001\026\001\027\001\028\001\
\002\001\030\001\031\001\255\255\006\001\255\255\255\255\255\255\
\255\255\011\001\012\001\255\255\255\255\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\255\255\023\001\024\001\025\001\
\026\001\027\001\028\001\002\001\030\001\031\001\255\255\006\001\
\255\255\255\255\255\255\255\255\011\001\012\001\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\006\001\
\023\001\024\001\025\001\026\001\027\001\028\001\002\001\030\001\
\031\001\016\001\006\001\002\001\255\255\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\016\001\255\255\018\001\019\001\
\031\001\016\001\255\255\018\001\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\031\001\255\255\255\255\255\255\255\255\
\031\001"

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
  OR\000\
  AND\000\
  NOT\000\
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
# 85 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 333 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 344 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 98 "parser.mly"
  ( set_fields _1 )
# 351 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 103 "parser.mly"
  ( [_1] )
# 358 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 105 "parser.mly"
  (_3 :: _1 )
# 366 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 378 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
  ( NOP )
# 384 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 120 "parser.mly"
  ( SEQ(_1, _2) )
# 392 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
        ( [] )
# 398 "parser.ml"
               : 'elseif_branches))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'elseif_branches) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 126 "parser.mly"
        ( (_3, _5) :: _1 )
# 407 "parser.ml"
               : 'elseif_branches))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'elseif_branches) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 131 "parser.mly"
        ( 
          let rec build_if cond then_branch elseifs else_branch =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, else_branch)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest else_branch)
          in
          build_if _2 _4 _5 _7
        )
# 424 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elseif_branches) in
    Obj.repr(
# 140 "parser.mly"
        (
          let rec build_if cond then_branch elseifs =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, NOP)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest)
          in
          build_if _2 _4 _5
        )
# 440 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 149 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 452 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 155 "parser.mly"
  (
			(* declare the variable (assign a register) and build SET_VAR *)
            let r = declare_var _1 in
            SET_VAR (r, _3)
		)
# 464 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 164 "parser.mly"
        ( NOT _2 )
# 471 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 166 "parser.mly"
        ( AND(_1, _3) )
# 479 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 168 "parser.mly"
        ( OR(_1, _3) )
# 487 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 170 "parser.mly"
  ( COMP(COMP_LT,_1, _3) )
# 495 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 172 "parser.mly"
  ( COMP(COMP_GT,_1, _3) )
# 503 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 174 "parser.mly"
  ( COMP(COMP_LE,_1, _3) )
# 511 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 176 "parser.mly"
  ( COMP(COMP_GE,_1, _3) )
# 519 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 178 "parser.mly"
  ( COMP(COMP_EQ,_1, _3) )
# 527 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 180 "parser.mly"
  ( COMP(COMP_NE,_1, _3) )
# 535 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'condition) in
    Obj.repr(
# 182 "parser.mly"
        ( _2 )
# 542 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 188 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "y out of range";
			(_2, _4)
		)
# 554 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 197 "parser.mly"
        ( CST(_1) )
# 561 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "parser.mly"
        ( let r = get_var _1 in
          if r = -1 then error (sprintf "undeclared variable %s" _1)
          else VAR r )
# 570 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 203 "parser.mly"
        ( CELL(0,fst _1, snd _1) )
# 577 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 205 "parser.mly"
        ( _2 )
# 584 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 207 "parser.mly"
        ( NEG(_2) )
# 591 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 209 "parser.mly"
        ( BINOP(OP_ADD, _1, _3) )
# 599 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 211 "parser.mly"
        ( BINOP(OP_SUB, _1, _3) )
# 607 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 213 "parser.mly"
        ( BINOP(OP_MUL, _1, _3) )
# 615 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 215 "parser.mly"
        ( BINOP(OP_DIV, _1, _3) )
# 623 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 217 "parser.mly"
        ( BINOP(OP_MOD, _1, _3) )
# 631 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 219 "parser.mly"
        ( _2 )
# 638 "parser.ml"
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
