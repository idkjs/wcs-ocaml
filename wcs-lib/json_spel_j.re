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
open Json_spel_t;
open Spel_j;

type result('a, 'b) = Spel_j.result('a, 'b) = | Ok('a) | Error('b);

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

let rec json_spel_to_yojson: json_spel => Json_t.safe =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(x) =>
      `List([
        `String("Assoc"),
        (
          x =>
            `List(
              List.map(
                ((arg0, arg1)) =>
                  `List([
                    (x => `String(x))(arg0),
                    (x => json_spel_to_yojson(x))(arg1),
                  ]),
                x,
              ),
            )
        )(
          x,
        ),
      ])
    | `Bool(x) => `List([`String("Bool"), (x => `Bool(x))(x)])
    | `Float(x) => `List([`String("Float"), (x => `Float(x))(x)])
    | `Int(x) => `List([`String("Int"), (x => `Int(x))(x)])
    | `List(x) =>
      `List([
        `String("List"),
        (x => `List(List.map(x => json_spel_to_yojson(x), x)))(x),
      ])
    | `Null => `List([`String("Null")])
    | `Expr(x) =>
      `List([`String("Expr"), (x => expression_to_yojson(x))(x)])
  )
and json_spel_of_yojson: Json_t.safe => error_or(json_spel) =
  [@ocaml.warning "-A"]
  (
    (json: Json_t.safe) =>
      switch (json) {
      | `List([`String("Assoc"), x]) =>
        (
          fun
          | `List(xs) =>
            map_bind(
              fun
              | `List([arg0, arg1]) =>
                (x => json_spel_of_yojson(x))(arg1)
                >>= (
                  arg1 =>
                    (
                      fun
                      | `String(x) => Ok(x)
                      | _ => Error("Spel_t.json_spel")
                    )(
                      arg0,
                    )
                    >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
                )
              | _ => Error("Spel_t.json_spel"),
              [],
              xs,
            )
          | _ => Error("Spel_t.json_spel")
        )(
          x,
        )
        >>= (x => Ok(`Assoc(x)))
      | `List([`String("Bool"), x]) =>
        (
          fun
          | `Bool(x) => Ok(x)
          | _ => Error("Spel_t.json_spel")
        )(x)
        >>= (x => Ok(`Bool(x)))
      | `List([`String("Float"), x]) =>
        (
          fun
          | `Int(x) => Ok(float_of_int(x))
          | `Intlit(x) => Ok(float_of_string(x))
          | `Float(x) => Ok(x)
          | _ => Error("Spel_t.json_spel")
        )(
          x,
        )
        >>= (x => Ok(`Float(x)))
      | `List([`String("Int"), x]) =>
        (
          fun
          | `Int(x) => Ok(x)
          | _ => Error("Spel_t.json_spel")
        )(x)
        >>= (x => Ok(`Int(x)))
      | `List([`String("List"), x]) =>
        (
          fun
          | `List(xs) => map_bind(x => json_spel_of_yojson(x), [], xs)
          | _ => Error("Spel_t.json_spel")
        )(
          x,
        )
        >>= (x => Ok(`List(x)))
      | `List([`String("Null")]) => Ok(`Null)
      | `List([`String("Expr"), x]) =>
        (x => expression_of_yojson(x))(x) >>= (x => Ok(`Expr(x)))
      | _ => Error("Spel_t.json_spel")
      }
  );
