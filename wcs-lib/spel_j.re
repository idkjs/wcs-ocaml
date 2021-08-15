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

type result('a, 'b) =
  | Ok('a)
  | Error('b);

let (>>=) = (x, f) =>
  switch (x) {
  | Ok(x) => f(x)
  | Error(_) as x => x
  };

let (>|=) = (x, f) => x >>= (x => Ok(f(x)));

let rec map_bind = (f, acc, xs) =>
  switch (xs) {
  | [x, ...xs] => f(x) >>= (x => map_bind(f, [x, ...acc], xs))
  | [] => Ok(List.rev(acc))
  };

type error_or('a) = result('a, string);

let location_to_yojson = pos => `Null;
let location_of_yojson = j =>
  [@implicit_arity]
  Ok(
    {
      Lexing.pos_fname: "",
      Lexing.pos_lnum: 0,
      Lexing.pos_bol: 0,
      Lexing.pos_cnum: 0,
    },
    {
      Lexing.pos_fname: "",
      Lexing.pos_lnum: 0,
      Lexing.pos_bol: 0,
      Lexing.pos_cnum: 0,
    },
  );
let rec spel_type_to_yojson: spel_type => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    fun
    | T_string => `List([`String("T_string")])
    | T_int => `List([`String("T_int")])
    | T_real => `List([`String("T_real")])
    | T_boolean => `List([`String("T_boolean")])
    | T_object => `List([`String("T_object")])
  )
and spel_type_of_yojson: Json_t.safe => error_or(spel_type) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("T_string")]) => Ok(T_string)
    | `List([`String("T_int")]) => Ok(T_int)
    | `List([`String("T_real")]) => Ok(T_real)
    | `List([`String("T_boolean")]) => Ok(T_boolean)
    | `List([`String("T_object")]) => Ok(T_object)
    | _ => Error("Spel_t.spel_type")
  );
let rec literal_to_yojson: literal => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    fun
    | L_string(arg0) =>
      `List([`String("L_string"), (x => `String(x))(arg0)])
    | L_int(arg0) => `List([`String("L_int"), (x => `Int(x))(arg0)])
    | L_real(arg0) => `List([`String("L_real"), (x => `Float(x))(arg0)])
    | L_boolean(arg0) =>
      `List([`String("L_boolean"), (x => `Bool(x))(arg0)])
    | L_null => `List([`String("L_null")])
  )
and literal_of_yojson: Json_t.safe => error_or(literal) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("L_string"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Spel_t.literal")
      )(arg0)
      >>= (arg0 => Ok(L_string(arg0)))
    | `List([`String("L_int"), arg0]) =>
      (
        fun
        | `Int(x) => Ok(x)
        | _ => Error("Spel_t.literal")
      )(arg0)
      >>= (arg0 => Ok(L_int(arg0)))
    | `List([`String("L_real"), arg0]) =>
      (
        fun
        | `Int(x) => Ok(float_of_int(x))
        | `Intlit(x) => Ok(float_of_string(x))
        | `Float(x) => Ok(x)
        | _ => Error("Spel_t.literal")
      )(
        arg0,
      )
      >>= (arg0 => Ok(L_real(arg0)))
    | `List([`String("L_boolean"), arg0]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Spel_t.literal")
      )(arg0)
      >>= (arg0 => Ok(L_boolean(arg0)))
    | `List([`String("L_null")]) => Ok(L_null)
    | _ => Error("Spel_t.literal")
  );
let rec op_to_yojson: op => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    fun
    | Op_eq => `List([`String("Op_eq")])
    | Op_ne => `List([`String("Op_ne")])
    | Op_lt => `List([`String("Op_lt")])
    | Op_le => `List([`String("Op_le")])
    | Op_gt => `List([`String("Op_gt")])
    | Op_ge => `List([`String("Op_ge")])
    | Op_not => `List([`String("Op_not")])
    | Op_and => `List([`String("Op_and")])
    | Op_or => `List([`String("Op_or")])
    | Op_plus => `List([`String("Op_plus")])
    | Op_minus => `List([`String("Op_minus")])
    | Op_uminus => `List([`String("Op_uminus")])
    | Op_mult => `List([`String("Op_mult")])
    | Op_div => `List([`String("Op_div")])
    | Op_mod => `List([`String("Op_mod")])
    | Op_concat => `List([`String("Op_concat")])
    | Op_toString => `List([`String("Op_toString")])
  )
and op_of_yojson: Json_t.safe => error_or(op) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("Op_eq")]) => Ok(Op_eq)
    | `List([`String("Op_ne")]) => Ok(Op_ne)
    | `List([`String("Op_lt")]) => Ok(Op_lt)
    | `List([`String("Op_le")]) => Ok(Op_le)
    | `List([`String("Op_gt")]) => Ok(Op_gt)
    | `List([`String("Op_ge")]) => Ok(Op_ge)
    | `List([`String("Op_not")]) => Ok(Op_not)
    | `List([`String("Op_and")]) => Ok(Op_and)
    | `List([`String("Op_or")]) => Ok(Op_or)
    | `List([`String("Op_plus")]) => Ok(Op_plus)
    | `List([`String("Op_minus")]) => Ok(Op_minus)
    | `List([`String("Op_uminus")]) => Ok(Op_uminus)
    | `List([`String("Op_mult")]) => Ok(Op_mult)
    | `List([`String("Op_div")]) => Ok(Op_div)
    | `List([`String("Op_mod")]) => Ok(Op_mod)
    | `List([`String("Op_concat")]) => Ok(Op_concat)
    | `List([`String("Op_toString")]) => Ok(Op_toString)
    | _ => Error("Spel_t.op")
  );
let rec expression_to_yojson: expression => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields = [
        (
          "expr_text",
          (
            fun
            | None => `Null
            | Some(x) => (x => `String(x))(x)
          )(
            x.expr_text,
          ),
        ),
        ...fields,
      ];
      let fields = [
        ("expr_loc", (x => location_to_yojson(x))(x.expr_loc)),
        ...fields,
      ];
      let fields = [
        ("expr_desc", (x => expression_desc_to_yojson(x))(x.expr_desc)),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and expression_of_yojson: Json_t.safe => error_or(expression) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1, arg2) as _state) =>
          switch (xs) {
          | [("expr_desc", x), ...xs] =>
            loop(xs, ((x => expression_desc_of_yojson(x))(x), arg1, arg2))
          | [("expr_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x), arg2))
          | [("expr_text", x), ...xs] =>
            loop(
              xs,
              (
                arg0,
                arg1,
                (
                  fun
                  | `Null => Ok(None)
                  | x =>
                    (
                      fun
                      | `String(x) => Ok(x)
                      | _ => Error("Spel_t.expression.expr_text")
                    )(
                      x,
                    )
                    >>= (x => Ok(Some(x)))
                )(
                  x,
                ),
              ),
            )
          | [] =>
            arg2
            >>= (
              arg2 =>
                arg1
                >>= (
                  arg1 =>
                    arg0
                    >>= (
                      arg0 =>
                        Ok({expr_desc: arg0, expr_loc: arg1, expr_text: arg2})
                    )
                )
            )
          | [_, ...xs] => Error("Spel_t.expression")
          };
        loop(
          xs,
          (
            Error("Spel_t.expression.expr_desc"),
            Error("Spel_t.expression.expr_loc"),
            Error("Spel_t.expression.expr_text"),
          ),
        );
      }
    | _ => Error("Spel_t.expression")
  )
and expression_desc_to_yojson: expression_desc => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    fun
    | E_lit(arg0) =>
      `List([`String("E_lit"), (x => literal_to_yojson(x))(arg0)])
    | [@implicit_arity] E_prop(arg0, arg1) =>
      `List([
        `String("E_prop"),
        (x => expression_to_yojson(x))(arg0),
        (x => `String(x))(arg1),
      ])
    | [@implicit_arity] E_prop_catch(arg0, arg1) =>
      `List([
        `String("E_prop_catch"),
        (x => expression_to_yojson(x))(arg0),
        (x => `String(x))(arg1),
      ])
    | [@implicit_arity] E_get(arg0, arg1) =>
      `List([
        `String("E_get"),
        (x => expression_to_yojson(x))(arg0),
        (x => expression_to_yojson(x))(arg1),
      ])
    | E_list(arg0) =>
      `List([
        `String("E_list"),
        (x => `List(List.map(x => expression_to_yojson(x), x)))(arg0),
      ])
    | [@implicit_arity] E_new_array(arg0, arg1, arg2) =>
      `List([
        `String("E_new_array"),
        (x => spel_type_to_yojson(x))(arg0),
        (
          x =>
            `List(
              List.map(
                fun
                | None => `Null
                | Some(x) => (x => `Int(x))(x),
                x,
              ),
            )
        )(
          arg1,
        ),
        (
          fun
          | None => `Null
          | Some(x) =>
            (x => `List(List.map(x => expression_to_yojson(x), x)))(x)
        )(
          arg2,
        ),
      ])
    | [@implicit_arity] E_new(arg0, arg1) =>
      `List([
        `String("E_new"),
        (x => `String(x))(arg0),
        (x => `List(List.map(x => expression_to_yojson(x), x)))(arg1),
      ])
    | [@implicit_arity] E_call(arg0, arg1, arg2) =>
      `List([
        `String("E_call"),
        (
          fun
          | None => `Null
          | Some(x) => (x => expression_to_yojson(x))(x)
        )(
          arg0,
        ),
        (x => `String(x))(arg1),
        (x => `List(List.map(x => expression_to_yojson(x), x)))(arg2),
      ])
    | [@implicit_arity] E_call_catch(arg0, arg1, arg2) =>
      `List([
        `String("E_call_catch"),
        (
          fun
          | None => `Null
          | Some(x) => (x => expression_to_yojson(x))(x)
        )(
          arg0,
        ),
        (x => `String(x))(arg1),
        (x => `List(List.map(x => expression_to_yojson(x), x)))(arg2),
      ])
    | [@implicit_arity] E_op(arg0, arg1) =>
      `List([
        `String("E_op"),
        (x => op_to_yojson(x))(arg0),
        (x => `List(List.map(x => expression_to_yojson(x), x)))(arg1),
      ])
    | [@implicit_arity] E_conditional(arg0, arg1, arg2) =>
      `List([
        `String("E_conditional"),
        (x => expression_to_yojson(x))(arg0),
        (x => expression_to_yojson(x))(arg1),
        (x => expression_to_yojson(x))(arg2),
      ])
    | E_ident(arg0) =>
      `List([`String("E_ident"), (x => `String(x))(arg0)])
    | E_anything_else => `List([`String("E_anything_else")])
    | E_context => `List([`String("E_context")])
    | E_conversation_start => `List([`String("E_conversation_start")])
    | E_entities => `List([`String("E_entities")])
    | E_input => `List([`String("E_input")])
    | E_intents => `List([`String("E_intents")])
    | E_output => `List([`String("E_output")])
    | E_variable(arg0) =>
      `List([
        `String("E_variable"),
        (
          ((arg0, arg1)) =>
            `List([
              (x => `String(x))(arg0),
              (
                fun
                | None => `Null
                | Some(x) => (x => `String(x))(x)
              )(arg1),
            ])
        )(
          arg0,
        ),
      ])
    | E_intent(arg0) =>
      `List([`String("E_intent"), (x => `String(x))(arg0)])
    | E_entity(arg0) =>
      `List([
        `String("E_entity"),
        (
          ((arg0, arg1)) =>
            `List([
              (x => `String(x))(arg0),
              (
                fun
                | None => `Null
                | Some(x) => (x => `String(x))(x)
              )(arg1),
            ])
        )(
          arg0,
        ),
      ])
    | E_error(arg0) => `List([`String("E_error"), `String(arg0)])
  )
and expression_desc_of_yojson: Json_t.safe => error_or(expression_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("E_lit"), arg0]) =>
      (x => literal_of_yojson(x))(arg0) >>= (arg0 => Ok(E_lit(arg0)))
    | `List([`String("E_prop"), arg0, arg1]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (x => expression_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_prop(arg0, arg1)))
      )
    | `List([`String("E_prop_catch"), arg0, arg1]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (x => expression_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_prop_catch(arg0, arg1)))
      )
    | `List([`String("E_get"), arg0, arg1]) =>
      (x => expression_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (x => expression_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_get(arg0, arg1)))
      )
    | `List([`String("E_list"), arg0]) =>
      (
        fun
        | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg0,
      )
      >>= (arg0 => Ok(E_list(arg0)))
    | `List([`String("E_new_array"), arg0, arg1, arg2]) =>
      (
        fun
        | `Null => Ok(None)
        | x =>
          (
            fun
            | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
            | _ => Error("Spel_t.expression_desc")
          )(
            x,
          )
          >>= (x => Ok(Some(x)))
      )(
        arg2,
      )
      >>= (
        arg2 =>
          (
            fun
            | `List(xs) =>
              map_bind(
                fun
                | `Null => Ok(None)
                | x =>
                  (
                    fun
                    | `Int(x) => Ok(x)
                    | _ => Error("Spel_t.expression_desc")
                  )(
                    x,
                  )
                  >>= (x => Ok(Some(x))),
                [],
                xs,
              )
            | _ => Error("Spel_t.expression_desc")
          )(
            arg1,
          )
          >>= (
            arg1 =>
              (x => spel_type_of_yojson(x))(arg0)
              >>= (
                arg0 => Ok([@implicit_arity] E_new_array(arg0, arg1, arg2))
              )
          )
      )
    | `List([`String("E_new"), arg0, arg1]) =>
      (
        fun
        | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (
            fun
            | `String(x) => Ok(x)
            | _ => Error("Spel_t.expression_desc")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] E_new(arg0, arg1)))
      )
    | `List([`String("E_call"), arg0, arg1, arg2]) =>
      (
        fun
        | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg2,
      )
      >>= (
        arg2 =>
          (
            fun
            | `String(x) => Ok(x)
            | _ => Error("Spel_t.expression_desc")
          )(
            arg1,
          )
          >>= (
            arg1 =>
              (
                fun
                | `Null => Ok(None)
                | x =>
                  (x => expression_of_yojson(x))(x) >>= (x => Ok(Some(x)))
              )(
                arg0,
              )
              >>= (arg0 => Ok([@implicit_arity] E_call(arg0, arg1, arg2)))
          )
      )
    | `List([`String("E_call_catch"), arg0, arg1, arg2]) =>
      (
        fun
        | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg2,
      )
      >>= (
        arg2 =>
          (
            fun
            | `String(x) => Ok(x)
            | _ => Error("Spel_t.expression_desc")
          )(
            arg1,
          )
          >>= (
            arg1 =>
              (
                fun
                | `Null => Ok(None)
                | x =>
                  (x => expression_of_yojson(x))(x) >>= (x => Ok(Some(x)))
              )(
                arg0,
              )
              >>= (
                arg0 => Ok([@implicit_arity] E_call_catch(arg0, arg1, arg2))
              )
          )
      )
    | `List([`String("E_op"), arg0, arg1]) =>
      (
        fun
        | `List(xs) => map_bind(x => expression_of_yojson(x), [], xs)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (x => op_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_op(arg0, arg1)))
      )
    | `List([`String("E_conditional"), arg0, arg1, arg2]) =>
      (x => expression_of_yojson(x))(arg2)
      >>= (
        arg2 =>
          (x => expression_of_yojson(x))(arg1)
          >>= (
            arg1 =>
              (x => expression_of_yojson(x))(arg0)
              >>= (
                arg0 => Ok([@implicit_arity] E_conditional(arg0, arg1, arg2))
              )
          )
      )
    | `List([`String("E_ident"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg0,
      )
      >>= (arg0 => Ok(E_ident(arg0)))
    | `List([`String("E_anything_else")]) => Ok(E_anything_else)
    | `List([`String("E_context")]) => Ok(E_context)
    | `List([`String("E_conversation_start")]) => Ok(E_conversation_start)
    | `List([`String("E_entities")]) => Ok(E_entities)
    | `List([`String("E_input")]) => Ok(E_input)
    | `List([`String("E_intents")]) => Ok(E_intents)
    | `List([`String("E_output")]) => Ok(E_output)
    | `List([`String("E_variable"), arg0]) =>
      (
        fun
        | `List([arg0, arg1]) =>
          (
            fun
            | `Null => Ok(None)
            | x =>
              (
                fun
                | `String(x) => Ok(x)
                | _ => Error("Spel_t.expression_desc")
              )(
                x,
              )
              >>= (x => Ok(Some(x)))
          )(
            arg1,
          )
          >>= (
            arg1 =>
              (
                fun
                | `String(x) => Ok(x)
                | _ => Error("Spel_t.expression_desc")
              )(
                arg0,
              )
              >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
          )
        | _ => Error("Spel_t.expression_desc")
      )(
        arg0,
      )
      >>= (arg0 => Ok(E_variable(arg0)))
    | `List([`String("E_intent"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Spel_t.expression_desc")
      )(
        arg0,
      )
      >>= (arg0 => Ok(E_intent(arg0)))
    | `List([`String("E_entity"), arg0]) =>
      (
        fun
        | `List([arg0, arg1]) =>
          (
            fun
            | `Null => Ok(None)
            | x =>
              (
                fun
                | `String(x) => Ok(x)
                | _ => Error("Spel_t.expression_desc")
              )(
                x,
              )
              >>= (x => Ok(Some(x)))
          )(
            arg1,
          )
          >>= (
            arg1 =>
              (
                fun
                | `String(x) => Ok(x)
                | _ => Error("Spel_t.expression_desc")
              )(
                arg0,
              )
              >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
          )
        | _ => Error("Spel_t.expression_desc")
      )(
        arg0,
      )
      >>= (arg0 => Ok(E_entity(arg0)))
    | `List([`String("E_error"), `String(arg0)]) =>
      (x => Ok(x))(arg0) >>= (arg0 => Ok(E_error(arg0)))
    | _ => Error("Spel_t.expression_desc")
  );
