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
open Spel_sedlexer_j;

exception LexError(string);

let error = (default, msg) => Log.error("Spel parsing", default, msg);
let warning = (default, msg) => {
  Log.warning("Spel parsing", msg);
  default;
};

type state =
  | BodyLexing
  | ExprLexing;

/** {6 Expression lexer} */

let expr_lexer = (buff, lb) => Spel_sedlexer_j.token(buff, lb);

let mk_expr_lexer = () => expr_lexer(Spel_util.string_buff());

/** {6 lexing} */

let body_lexer = (st, buff, lb) =>
  switch (st^) {
  | BodyLexing =>
    let tk = Spel_sedlexer_j.body(buff, lb);
    switch (tk) {
    | Spel_parser_j.OPENEXPR(_) =>
      st := ExprLexing;
      tk;
    | _ => tk
    };
  | ExprLexing =>
    let tk = Spel_sedlexer_j.token(buff, lb);
    switch (tk) {
    | Spel_parser_j.CLOSEEXPR =>
      st := BodyLexing;
      tk;
    | _ => tk
    };
  };

let mk_body_lexer = () =>
  body_lexer(ref(BodyLexing), Spel_util.string_buff());

/** {6 desugaring} */;

let rec fold_expr = (f, e) => {
  let desc =
    switch (e.expr_desc) {
    | E_lit(l) => E_lit(l)
    | [@implicit_arity] E_prop(e, s) =>
      [@implicit_arity] E_prop(fold_expr(f, e), s)
    | [@implicit_arity] E_prop_catch(e, s) =>
      [@implicit_arity] E_prop_catch(fold_expr(f, e), s)
    | [@implicit_arity] E_get(e1, e2) =>
      [@implicit_arity] E_get(fold_expr(f, e1), fold_expr(f, e2))
    | E_list(el) => E_list(List.map(fold_expr(f), el))
    | [@implicit_arity] E_new_array(t, ilo, elo) =>
      switch (elo) {
      | None => [@implicit_arity] E_new_array(t, ilo, None)
      | Some(el) =>
        [@implicit_arity]
        E_new_array(t, ilo, Some(List.map(fold_expr(f), el)))
      }
    | [@implicit_arity] E_new(t, el) =>
      [@implicit_arity] E_new(t, List.map(fold_expr(f), el))
    | [@implicit_arity] E_call(eo, s, el) =>
      switch (eo) {
      | None =>
        [@implicit_arity] E_call(None, s, List.map(fold_expr(f), el))
      | Some(e) =>
        [@implicit_arity]
        E_call(Some(fold_expr(f, e)), s, List.map(fold_expr(f), el))
      }
    | [@implicit_arity] E_call_catch(eo, s, el) =>
      switch (eo) {
      | None =>
        [@implicit_arity] E_call_catch(None, s, List.map(fold_expr(f), el))
      | Some(e) =>
        [@implicit_arity]
        E_call_catch(Some(fold_expr(f, e)), s, List.map(fold_expr(f), el))
      }
    | [@implicit_arity] E_op(op, el) =>
      [@implicit_arity] E_op(op, List.map(fold_expr(f), el))
    | [@implicit_arity] E_conditional(e1, e2, e3) =>
      [@implicit_arity]
      E_conditional(fold_expr(f, e1), fold_expr(f, e2), fold_expr(f, e3))
    | E_ident(s) => E_ident(s)
    /* WCS extensions */
    | E_anything_else => E_anything_else
    | E_context => E_context
    | E_conversation_start => E_conversation_start
    | E_entities => E_entities
    | E_input => E_input
    | E_intents => E_intents
    | E_output => E_output
    | E_variable(s) => E_variable(s)
    | E_intent(s) => E_intent(s)
    | [@implicit_arity] E_entity(s1, s2) =>
      [@implicit_arity] E_entity(s1, s2)
    /* Fallback */
    | E_error(s) => E_error(s)
    };

  let desc = f(desc);
  let loc = e.expr_loc;
  let text = e.expr_text;
  Spel_util.mk_expr_full(desc, loc, text);
};

let refresh_expr_text = e =>
  switch (e.expr_text) {
  | None => ()
  | Some(t) => e.expr_text = Some(Spel_print.to_string(e))
  };

let desugar_spel = ref(false);

let desugar_desc = e =>
  switch (e) {
  /* Shorthand syntax for variables */
  | [@implicit_arity] E_variable(v, None) =>
    [@implicit_arity]
    E_get(
      Spel_util.mk_expr(E_context),
      Spel_util.mk_expr(E_lit(L_string(v))),
    )
  | [@implicit_arity] E_variable(v, Some(s)) =>
    [@implicit_arity]
    E_op(
      Op_eq,
      [
        Spel_util.mk_expr(
          [@implicit_arity]
          E_get(
            Spel_util.mk_expr(E_context),
            Spel_util.mk_expr(E_lit(L_string(v))),
          ),
        ),
        Spel_util.mk_expr(E_lit(L_string(s))),
      ],
    )
  | _ => e
  };

let desugar = e => fold_expr(desugar_desc, e);

let resugar_spel = ref(false);

let resugar_desc = e =>
  switch (e) {
  /* Shorthand syntax for variables */
  | [@implicit_arity]
    E_get({expr_desc: E_context}, {expr_desc: E_lit(L_string(s))}) =>
    [@implicit_arity] E_variable(s, None)
  /* XXX Keeps equality intact when resugaring
     | E_op (Op_eq,
          [{ expr_desc = E_get ({ expr_desc = E_context },
                                { expr_desc = (E_lit (L_string v)) }) };
           { expr_desc = E_lit (L_string s) }]) ->
      E_variable (v, Some s) XXX */
  | _ => e
  };

let resugar = e => fold_expr(resugar_desc, e);

let sugarer = e => {
  let e =
    if (desugar_spel^) {
      desugar(e);
    } else {
      e;
    };
  let e =
    if (resugar_spel^) {
      resugar(e);
    } else {
      e;
    };
  e;
};

/** {6 parsers} */;

let fix_empty_condition = ocond =>
  switch (ocond) {
  | Some(cond) => cond
  | None => Spel_util.mk_expr(E_lit(L_boolean(false)))
  };

let expression_from_file = f =>
  fix_empty_condition(
    Spel_util.uparse_file(Spel_parser_j.condition_main, mk_expr_lexer(), f),
  );

let expr_from_file = s => sugarer(expression_from_file(s));

let expression_from_string = s =>
  try({
    let parsed =
      Spel_util.uparse_string(
        Spel_parser_j.condition_main,
        mk_expr_lexer(),
        s,
      );

    let ast = fix_empty_condition(parsed);
    ast.expr_text = Some(s);
    ast;
  }) {
  | LexError(msg) =>
    warning(
      Spel_util.mk_expr_text(E_error(msg), Some(s)),
      Format.sprintf("[%s] in expression: '%s'", msg, s),
    )
  | _ =>
    warning(
      Spel_util.mk_expr_text(E_error("Parse error in expression"), Some(s)),
      Format.sprintf("error in expression: '%s'", s),
    )
  };

let expr_from_string = s => sugarer(expression_from_string(s));

let expression_from_quoted_file = f =>
  Spel_util.uparse_file(Spel_parser_j.body_main, mk_body_lexer(), f);

let quoted_expr_from_file = s => sugarer(expression_from_quoted_file(s));

let expression_from_quoted_string = s =>
  try({
    let ast =
      Spel_util.uparse_string(Spel_parser_j.body_main, mk_body_lexer(), s);

    ast.expr_text = Some(s);
    ast;
  }) {
  | LexError(msg) =>
    warning(
      Spel_util.mk_expr_text(E_error(msg), Some(s)),
      Format.sprintf("[%s] in text: '%s'", msg, s),
    )
  | _ =>
    warning(
      Spel_util.mk_expr_text(E_error("Parse error in text"), Some(s)),
      Format.sprintf("in text: '%s'", s),
    )
  };

let quoted_expr_from_string = s => sugarer(expression_from_quoted_string(s));
