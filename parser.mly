/*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

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

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

/* symbols */
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token DOT_DOT
%token DOT
%token NEG
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token IF THEN ELSE ELSEIF
%token LT GT LE GE EQ NE
%token LPAREN RPAREN
/* values */
%token <string> ID
%token<int> INT

%left ADD SUB
%left MUL DIV MOD

%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;

opt_statements:
	/* empty */
		{ NOP }
|	opt_statements statement
		{ SEQ($1, $2) }
;
elseif_branches:
    /* empty */
        { [] }
  | elseif_branches ELSEIF condition THEN opt_statements
        { ($3, $5) :: $1 }


statement:
	IF condition THEN opt_statements elseif_branches ELSE opt_statements END
        { 
          let rec build_if cond then_branch elseifs else_branch =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, else_branch)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest else_branch)
          in
          build_if $2 $4 $5 $7
        }
  | IF condition THEN opt_statements elseif_branches END
        {
          let rec build_if cond then_branch elseifs =
            match elseifs with
            | [] -> IF_THEN(cond, then_branch, NOP)
            | (ec, eb)::rest -> IF_THEN(cond, then_branch, build_if ec eb rest)
          in
          build_if $2 $4 $5
        }
|	cell ASSIGN expression
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN expression
		{
			(* declare the variable (assign a register) and build SET_VAR *)
            let r = declare_var $1 in
            SET_VAR (r, $3)
		}
;
condition:
	expression LT expression
		{ COMP(COMP_LT,$1, $3) }
  | expression GT expression
		{ COMP(COMP_GT,$1, $3) }
  | expression LE expression
		{ COMP(COMP_LE,$1, $3) }
  | expression GE expression
		{ COMP(COMP_GE,$1, $3) }
  | expression EQ expression
		{ COMP(COMP_EQ,$1, $3) }
  | expression NE expression
		{ COMP(COMP_NE,$1, $3) }


cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "y out of range";
			($2, $4)
		}
;

expression:
    INT
        { CST($1) }
  | ID
        { let r = get_var $1 in
          if r = -1 then error (sprintf "undeclared variable %s" $1)
          else VAR r }
  | cell
        { CELL(0,fst $1, snd $1) }
  | ADD expression
        { $2 }
  | SUB expression
        { NEG($2) }
  | expression ADD expression
        { BINOP(OP_ADD, $1, $3) }
  | expression SUB expression
        { BINOP(OP_SUB, $1, $3) }
  | expression MUL expression
        { BINOP(OP_MUL, $1, $3) }
  | expression DIV expression
        { BINOP(OP_DIV, $1, $3) }
  | expression MOD expression
        { BINOP(OP_MOD, $1, $3) }
  | LPAREN expression RPAREN
        { $2 }
;



