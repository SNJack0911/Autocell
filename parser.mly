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
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
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
		{ $2 }
;


statement:
	cell ASSIGN expressions
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN expressions
		{
			(* declare the variable (assign a register) and build SET_VAR *)
            let r = declare_var $1 in
            SET_VAR (r, $3)
		}
;



cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;

expressions:
	expression
		{ $1 }
|	expressions ADD expression
        { BINOP(OP_ADD, $1, $3) }
|	expressions SUB expression
        { BINOP(OP_SUB, $1, $3) }
|	expressions MUL expression
        { BINOP(OP_MUL, $1, $3) }
|	expressions DIV expression
        { BINOP(OP_DIV, $1, $3) }
|	expressions MOD expression
        { BINOP(OP_MOD, $1, $3) }
;	
expression:
	ADD expression
        { $2 }
|	SUB expression
        { NEG($2) }
|	cell
		{ printf "[%d,%d]\n" (fst $1) (snd $1); CELL (0, fst $1, snd $1) }
|	INT
		{ printf "%d\n" $1; CST $1 }
|	ID	
		{
            let r = get_var $1 in
            if r = -1 then error (sprintf "undeclared variable %s" $1)
            else VAR r
        }
|	LPAREN expressions RPAREN
		{ $2 }
;




