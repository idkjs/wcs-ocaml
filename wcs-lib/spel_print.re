/*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

open Spel_t;

let escape = (istop: bool, s) =>
  if (istop) {
    "<?" ++ s ++ "?>";
  } else {
    s;
  };

let paren = (prec_out, prec_in, s) =>
  if (prec_out > prec_in) {
    "(" ++ s ++ ")";
  } else {
    s;
  };

let recover = (msg, e) =>
  switch (e.expr_text) {
  | Some(text) => text
  | None => "[ERROR] " ++ msg
  };

let print_lit = (istop: bool, v): string =>
  switch (v) {
  | L_string(s) =>
    if (istop) {
      s;
    } else {
      "'" ++ s ++ "'";
    }
  | L_int(n) => string_of_int(n)
  | L_real(n) => string_of_float(n)
  | L_boolean(b) => if (b) {"true"} else {"false"}
  | L_null => "null"
  };

let print_type = (t): string =>
  switch (t) {
  | T_string => "String"
  | T_int => "int"
  | T_real => "double"
  | T_boolean => "boolean"
  | T_object => "Object"
  };

let op_prec = (op: Spel_t.op): int =>
  switch (op) {
  | Op_eq
  | Op_ne => 8
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge => 9
  | Op_not => 14
  | Op_and => 4
  | Op_or => 3
  | Op_plus
  | Op_minus => 11
  | Op_uminus => 14
  | Op_mult
  | Op_div
  | Op_mod => 12
  | Op_concat => 11
  | Op_toString => 14
  };

let print_op = (prec: int, op: Spel_t.op, l: list(string)): string =>
  switch (op, l) {
  | (Op_eq, [v1, v2]) => paren(prec, 8, v1 ++ " == " ++ v2)
  | (Op_ne, [v1, v2]) => paren(prec, 8, v1 ++ " != " ++ v2)
  | (Op_lt, [v1, v2]) => paren(prec, 9, v1 ++ " < " ++ v2)
  | (Op_le, [v1, v2]) => paren(prec, 9, v1 ++ " <= " ++ v2)
  | (Op_gt, [v1, v2]) => paren(prec, 9, v1 ++ " > " ++ v2)
  | (Op_ge, [v1, v2]) => paren(prec, 9, v1 ++ " >= " ++ v2)
  | (Op_not, [v]) => paren(prec, 14, "!" ++ v)
  | (Op_and, [v1, v2]) => paren(prec, 4, v1 ++ " && " ++ v2)
  | (Op_or, [v1, v2]) => paren(prec, 3, v1 ++ " || " ++ v2)
  | (Op_plus, [v1, v2]) => paren(prec, 11, v1 ++ " + " ++ v2)
  | (Op_minus, [v1, v2]) => paren(prec, 11, v1 ++ " - " ++ v2)
  | (Op_uminus, [v1]) => paren(prec, 14, "-" ++ v1)
  | (Op_mult, [v1, v2]) => paren(prec, 12, v1 ++ " * " ++ v2)
  | (Op_div, [v1, v2]) => paren(prec, 12, v1 ++ " / " ++ v2)
  | (Op_mod, [v1, v2]) => paren(prec, 12, v1 ++ " % " ++ v2)
  | (Op_concat, [v, ...l]) =>
    paren(prec, 11, List.fold_left((acc, v) => acc ++ " + " ++ v, v, l))
  | (Op_toString, [v1]) => paren(prec, 14, v1 ++ ".toString()")
  | _ => "[bad number of arguments for operator]"
  };

let print_dim = dim =>
  switch (dim) {
  | Some(d) => "[" ++ string_of_int(d) ++ "]"
  | None => "[]"
  };

let print_dims = (dims): string =>
  String.concat("", List.map(print_dim, dims));

let rec print_expr_aux = (prec: int, istop: bool, e): string =>
  switch (e.expr_desc) {
  | E_lit(v) => print_lit(istop, v)
  | [@implicit_arity] E_prop(e, x) =>
    paren(prec, 14, escape(istop, print_expr_aux(14, false, e) ++ "." ++ x))
  | [@implicit_arity] E_prop_catch(e, x) =>
    paren(
      prec,
      14,
      escape(istop, print_expr_aux(14, false, e) ++ "?." ++ x),
    )
  | E_list(l) =>
    escape(
      istop,
      "{"
      ++ String.concat(",", List.map(print_expr_aux(0, false), l))
      ++ "}",
    )
  | [@implicit_arity] E_get(e1, e2) =>
    escape(
      istop,
      print_expr_aux(0, false, e1)
      ++ "["
      ++ print_expr_aux(0, false, e2)
      ++ "]",
    )
  | [@implicit_arity] E_new_array(t, dims, Some(init)) =>
    paren(
      prec,
      13,
      escape(
        istop,
        "new "
        ++ print_type(t)
        ++ print_dims(dims)
        ++ "{"
        ++ String.concat(",", List.map(print_expr_aux(0, false), init))
        ++ "}",
      ),
    )
  | [@implicit_arity] E_new_array(t, dims, None) =>
    paren(
      prec,
      13,
      escape(istop, "new " ++ print_type(t) ++ print_dims(dims)),
    )
  | [@implicit_arity] E_new(x, el) =>
    paren(
      prec,
      13,
      escape(
        istop,
        "new"
        ++ x
        ++ "("
        ++ String.concat(",", List.map(print_expr_aux(0, false), el))
        ++ ")",
      ),
    )
  | [@implicit_arity] E_call(None, x, el) =>
    paren(
      prec,
      16,
      escape(
        istop,
        x
        ++ ".("
        ++ String.concat(",", List.map(print_expr_aux(0, false), el))
        ++ ")",
      ),
    )
  | [@implicit_arity] E_call(Some(e), x, el) =>
    paren(
      prec,
      16,
      escape(
        istop,
        print_expr_aux(16, false, e)
        ++ "."
        ++ x
        ++ "("
        ++ String.concat(",", List.map(print_expr_aux(0, false), el))
        ++ ")",
      ),
    )
  | [@implicit_arity] E_call_catch(None, x, el) =>
    paren(
      prec,
      16,
      escape(
        istop,
        x
        ++ ".("
        ++ String.concat(",", List.map(print_expr_aux(0, false), el))
        ++ ")",
      ),
    )
  | [@implicit_arity] E_call_catch(Some(e), x, el) =>
    paren(
      prec,
      16,
      escape(
        istop,
        print_expr_aux(16, false, e)
        ++ "?."
        ++ x
        ++ "("
        ++ String.concat(",", List.map(print_expr_aux(0, false), el))
        ++ ")",
      ),
    )
  | [@implicit_arity] E_op(Op_concat, [e1, e2]) =>
    if (istop) {
      print_expr_aux(0, istop, e1) ++ print_expr_aux(0, istop, e2);
    } else {
      paren(
        prec,
        11,
        print_expr_aux(11, istop, e1)
        ++ " + "
        ++ print_expr_aux(11, istop, e2),
      );
    }
  | [@implicit_arity] E_op(op, l) =>
    escape(
      istop,
      print_op(prec, op, List.map(print_expr_aux(op_prec(op), false), l)),
    )
  | [@implicit_arity] E_conditional(e1, e2, e3) =>
    paren(
      prec,
      2,
      print_expr_aux(2, istop, e1)
      ++ "?"
      ++ print_expr_aux(2, istop, e2)
      ++ ":"
      ++ print_expr_aux(2, istop, e3),
    )
  | E_ident(x) => x
  | E_anything_else => "anything_else"
  | E_context => "context"
  | E_conversation_start => "conversation_start"
  | E_entities => "entities"
  | E_input => "input"
  | E_intents => "intents"
  | E_output => "output"
  | [@implicit_arity] E_variable(x, None) => "$" ++ x
  | [@implicit_arity] E_variable(x, Some(y)) =>
    "$" ++ x ++ ":" ++ "(" ++ y ++ ")"
  | E_intent(x) => "#" ++ x
  | [@implicit_arity] E_entity(x, None) => "@" ++ x
  | [@implicit_arity] E_entity(x, Some(y)) =>
    "@" ++ x ++ ":" ++ "(" ++ y ++ ")"
  | E_error(msg) => recover(msg, e)
  };

let lift_constants = e => `String(e);

let print_expression_common = (istop, e) =>
  lift_constants(print_expr_aux(0, istop, e));

/** {6 Top level printer for Spel expressions} */

let to_string = (e): string => print_expr_aux(0, false, e);

/** {6 Auxiliary printer for text expressions} */

let to_text = e => print_expr_aux(0, true, e);
