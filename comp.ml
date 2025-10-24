(*
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
 *)

open Ast
open Cell
open Quad
open Symbols

(** Variable containing the current x position. *)
let x = 0

(** Variable containing the current y position. *)
let y = 1

(** Variable containing the width of the array. *)
let w = 2

(** Variable containing the height of the array. *)
let h = 3

(** Variable containing 1! *)
let one = 4

(** Compute the position from the relative offset.
	@param x	X offset.
	@param y	Y offset.
	@return		Corresponding position. *)
let pos x y =
	match (x, y) with
	| (0, 0)	-> pCENTER
	| (0, -1)	-> pNORTH
	| (-1, -1)	-> pNORTHWEST
	| (-1, 0)	-> pWEST
	| (-1, +1)	-> pSOUTHWEST
	| (0, +1)	-> pSOUTH
	| (+1, +1)	-> pSOUTHEAST
	| (+1, 0)	-> pEAST
	| (+1, -1)	-> pNORTHEAST
	| _			-> failwith "bad offsets"
	


(** Compile an expression.
	@param e	Expression to compile.
	@return		(register containing the result, quads producing the result). *)
let rec comp_expr e =
	match e with
    | NONE ->
        (0, [])
    | CELL (f, val1, val2) ->
        let value = new_reg () in
        (value, [
            INVOKE (cGET + f, value, pos val1 val2)
        ])
    | CST i ->
        let value = new_reg () in
        (value, [ SETI(value, i) ])
    | VAR r ->
        (* copy variable register content into a fresh register *)
        let value = new_reg () in
        (value, [ SET(value, r) ])
    | NEG e1 ->
        (* compute -e1 by subtracting from 0 *)
        let (value, quad) = comp_expr e1 in
        let zero = new_reg () in
        let v = new_reg () in
        (v, quad @ [ SETI(zero, 0); SUB(v, zero, value) ])
    | BINOP (op, e1, e2) ->
        let (val1, quad1) = comp_expr e1 in
        let (val2, quad2) = comp_expr e2 in
        let value = new_reg () in
        let op_quad =
            match op with
            | OP_ADD -> ADD(value, val1, val2)
            | OP_SUB -> SUB(value, val1, val2)
            | OP_MUL -> MUL(value, val1, val2)
            | OP_DIV -> DIV(value, val1, val2)
            | OP_MOD -> MOD(value, val1, val2)
        in
        (value, quad1 @ quad2 @ [op_quad])

(** Compile a condition.
	@param c		Condition to compile.
	@param l_then	Label to branch to when the condition is true.
	@param l_else	Label to branch to when the condition is false.
	@return			Quads implementing the condition. *)
let rec comp_cond c l_then l_else =
	match c with
	| COMP (op, e1, e2) ->
		let (val1, quad1) = comp_expr e1 in
		let (val2, quad2) = comp_expr e2 in
		let jump_quad =
			match op with
			| COMP_LT -> GOTO_LT(l_then, val1, val2)
			| COMP_GT -> GOTO_GT(l_then, val1, val2)
			| COMP_LE -> GOTO_LE(l_then, val1, val2)
			| COMP_GE -> GOTO_GE(l_then, val1, val2)
			| COMP_EQ -> GOTO_EQ(l_then, val1, val2)
			| COMP_NE -> GOTO_NE(l_then, val1, val2)
		in
		quad1 @ quad2 @ [
			jump_quad;
			GOTO(l_else)
		]
	| NOT c1 ->
			comp_cond c1 l_else l_then
	| AND (c1, c2) ->
			let l_mid = new_lab () in
			let q1 = comp_cond c1 l_mid l_else in
			let q2 = comp_cond c2 l_then l_else in
			q1 @ [
				LABEL(l_mid) 
				] @ q2
	| OR (c1, c2) ->
			let l_mid = new_lab () in
			let q1 = comp_cond c1 l_then l_mid in
			let q2 = comp_cond c2 l_then l_else in
			q1 @ [ LABEL(l_mid) ] @ q2
	| _ ->
		failwith "bad condition"


(** Compile a statement.
	@param s	Statement to compile.
	@return		Quads implementing the statement. *)
let rec comp_stmt s =
	match s with
	| NOP ->
		[]
	| IF_THEN (cond, stmt_then, stmt_else) ->
		let l_then = new_lab () in
		let l_else = new_lab () in
		let l_end = new_lab () in
		let cond_quad = comp_cond cond l_then l_else in
		let then_quad = comp_stmt stmt_then in
		let else_quad = comp_stmt stmt_else in
		cond_quad @ [
			LABEL(l_then);
		] @ then_quad @ [
			GOTO(l_end);
			LABEL(l_else)
		] @ else_quad @ [
			LABEL(l_end)
		]
	| SEQ (s1, s2) ->
		(comp_stmt s1) @ (comp_stmt s2)
	| SET_CELL (0, expr) ->
		let (value, quad) = comp_expr expr in
		quad @ [
			INVOKE (cSET, value, 0)
		]
	| SET_VAR (reg, expr) ->
			let (value, quad) = comp_expr expr in
			(* copy expression result into the variable register *)
			quad @ [ SET(reg, value) ]
	| _ ->
		failwith "bad instruction"

(** Compile the given application.
	@param flds		List of fields.
	@param stmt		Instructions.
	@return			List of quadss. *)	
let compile flds stmt =
	let x_lab = new_lab () in
	let y_lab = new_lab () in
	[
		INVOKE(cSIZE, w, h);
		SETI(one, 1);

		SETI(x, 0);
		LABEL x_lab;

		SETI(y, 0);
		LABEL y_lab;
		INVOKE(cMOVE, x, y)
	]
	@
	(comp_stmt stmt)
	@
	[
		ADD(y, y, one);
		GOTO_LT(y_lab, y, h);

		ADD(x, x, one);
		GOTO_LT(x_lab, x, w);
		STOP
	]
