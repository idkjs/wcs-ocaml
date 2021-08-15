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

open Wcs_lib;
open Json_t;
open Deriving_intf;
open Cnl_t;

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

type cnl_instr =
  | I_repl_expr(int, cnl_expr_desc)
  | I_repl_actn(int, cnl_actn_desc)
  | I_repl_evnt(int, cnl_evnt_desc)
  | I_repl_cond(int, cnl_cond_desc)
  | I_repl_actns(int, cnl_actns_desc)
  | I_repl_actns_closed(int, bool)
  | I_conf_expr(int, bool)
  | I_conf_actn(int, bool)
  | I_conf_evnt(int, bool)
  | I_conf_cond(int, bool)
  | I_conf_actns(int, bool)
  | I_conf_rule(int, bool)
  | I_insr_actn;
let rec cnl_instr_to_yojson: cnl_instr => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | [@implicit_arity] I_repl_expr(arg0, arg1) =>
      `List([
        `String("I_repl_expr"),
        (x => `Int(x))(arg0),
        (x => cnl_expr_desc_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] I_repl_actn(arg0, arg1) =>
      `List([
        `String("I_repl_actn"),
        (x => `Int(x))(arg0),
        (x => cnl_actn_desc_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] I_repl_evnt(arg0, arg1) =>
      `List([
        `String("I_repl_evnt"),
        (x => `Int(x))(arg0),
        (x => cnl_evnt_desc_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] I_repl_cond(arg0, arg1) =>
      `List([
        `String("I_repl_cond"),
        (x => `Int(x))(arg0),
        (x => cnl_cond_desc_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] I_repl_actns(arg0, arg1) =>
      `List([
        `String("I_repl_actns"),
        (x => `Int(x))(arg0),
        (x => cnl_actns_desc_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] I_repl_actns_closed(arg0, arg1) =>
      `List([
        `String("I_repl_actns_closed"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_expr(arg0, arg1) =>
      `List([
        `String("I_conf_expr"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_actn(arg0, arg1) =>
      `List([
        `String("I_conf_actn"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_evnt(arg0, arg1) =>
      `List([
        `String("I_conf_evnt"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_cond(arg0, arg1) =>
      `List([
        `String("I_conf_cond"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_actns(arg0, arg1) =>
      `List([
        `String("I_conf_actns"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | [@implicit_arity] I_conf_rule(arg0, arg1) =>
      `List([
        `String("I_conf_rule"),
        (x => `Int(x))(arg0),
        (x => `Bool(x))(arg1),
      ])
    | I_insr_actn => `List([`String("I_insr_actn")])
  )
and cnl_instr_of_yojson: Yojson.Safe.t => error_or(cnl_instr) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("I_repl_expr"), arg0, arg1]) =>
      (x => cnl_expr_desc_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_expr(arg0, arg1)))
      )
    | `List([`String("I_repl_actn"), arg0, arg1]) =>
      (x => cnl_actn_desc_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_actn(arg0, arg1)))
      )
    | `List([`String("I_repl_evnt"), arg0, arg1]) =>
      (x => cnl_evnt_desc_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_evnt(arg0, arg1)))
      )
    | `List([`String("I_repl_cond"), arg0, arg1]) =>
      (x => cnl_cond_desc_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_cond(arg0, arg1)))
      )
    | `List([`String("I_repl_actns"), arg0, arg1]) =>
      (x => cnl_actns_desc_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_actns(arg0, arg1)))
      )
    | `List([`String("I_repl_actns_closed"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_repl_actns_closed(arg0, arg1)))
      )
    | `List([`String("I_conf_expr"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_expr(arg0, arg1)))
      )
    | `List([`String("I_conf_actn"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_actn(arg0, arg1)))
      )
    | `List([`String("I_conf_evnt"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_evnt(arg0, arg1)))
      )
    | `List([`String("I_conf_cond"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_cond(arg0, arg1)))
      )
    | `List([`String("I_conf_actns"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_actns(arg0, arg1)))
      )
    | `List([`String("I_conf_rule"), arg0, arg1]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_instr_t.cnl_instr")
      )(arg1)
      >>= (
        arg1 =>
          (
            fun
            | `Int(x) => Ok(x)
            | _ => Error("Cnl_instr_t.cnl_instr")
          )(
            arg0,
          )
          >>= (arg0 => Ok([@implicit_arity] I_conf_rule(arg0, arg1)))
      )
    | `List([`String("I_insr_actn")]) => Ok(I_insr_actn)
    | _ => Error("Cnl_instr_t.cnl_instr")
  );
type cnl_program = list(cnl_instr);
let rec cnl_program_to_yojson: cnl_program => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (x => `List(List.map(x => cnl_instr_to_yojson(x), x)))
and cnl_program_of_yojson: Yojson.Safe.t => error_or(cnl_program) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List(xs) => map_bind(x => cnl_instr_of_yojson(x), [], xs)
    | _ => Error("Cnl_instr_t.cnl_program")
  );
let focus_of_instr = instr =>
  switch (instr) {
  | [@implicit_arity] I_repl_expr(focus, _) => focus
  | [@implicit_arity] I_repl_actn(focus, _) => focus
  | [@implicit_arity] I_repl_evnt(focus, _) => focus
  | [@implicit_arity] I_repl_cond(focus, _) => focus
  | [@implicit_arity] I_repl_actns(focus, _) => focus
  | [@implicit_arity] I_repl_actns_closed(focus, _) => focus
  | [@implicit_arity] I_conf_expr(focus, _) => focus
  | [@implicit_arity] I_conf_actn(focus, _) => focus
  | [@implicit_arity] I_conf_evnt(focus, _) => focus
  | [@implicit_arity] I_conf_cond(focus, _) => focus
  | [@implicit_arity] I_conf_actns(focus, _) => focus
  | [@implicit_arity] I_conf_rule(focus, _) => focus
  | I_insr_actn => assert(false)
  };
