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
open Wcs_t;
open Cnl_t;
open Cnl_util;
open Dialog_util;
open Call_t;

/** {6. Utils} */;

let null: json = (`Null: json);

let set = (ctx: json, lbl: string, v: json): json =>
  switch (ctx) {
  | `Null => `Assoc([(lbl, v)])
  | `Assoc(l) => `Assoc([(lbl, v), ...List.remove_assoc(lbl, l)])
  | _ => raise(Failure("Unable to add a property to a non-object value"))
  };

let take = (ctx: json, lbl: string): (json, option(json)) =>
  switch (ctx) {
  | `Assoc(l) =>
    try({
      let v = List.assoc(lbl, l);
      (`Assoc(List.remove_assoc(lbl, l)), Some(v));
    }) {
    | Not_found => (ctx, None)
    }
  | _ => (ctx, None)
  };

let get = (ctx: json, lbl: string): option(json) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | x => Some(x)
  };

/** {6. skip_user_input} */;

let set_skip_user_input = (ctx: json, b: bool): json =>
  set(ctx, "skip_user_input", `Bool(b));

let take_skip_user_input = (ctx: json): (json, bool) =>
  switch (take(ctx, "skip_user_input")) {
  | (ctx, Some(`Bool(b))) => (ctx, b)
  | _ => (ctx, false)
  };

/** {6. Call} */;

let set_call = (ctx: json, c: call): json =>
  set(ctx, "call", Yojson.Basic.from_string(Call_j.string_of_call(c)));

let take_call = (ctx: json): (json, option(call)) =>
  switch (take(ctx, "call")) {
  | (ctx', Some(c)) =>
    try((ctx', Some(Call_j.call_of_string(Yojson.Basic.to_string(c))))) {
    | _ =>
      Format.eprintf(
        "[WARNING] illed formed call:\n%s@.",
        Yojson.Basic.pretty_to_string(c),
      );
      (ctx, None);
    }
  | _ => (ctx, None)
  };

/** {6. Return} */;

let set_return = (ctx: json, x: json): json => set(ctx, "return", x);

let get_return = (ctx: json): option(json) => get(ctx, "return");

/** {6. Rule} */;

let set_rule = (ctx: json, lbl: string, rule: cnl_rule): json =>
  set(ctx, lbl, json_of_rule(rule));

let get_rule = (ctx: json, lbl: string): option(cnl_rule) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | rule =>
    switch (Cnl_t.cnl_rule_of_yojson((rule :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Expr} */;

let set_expr = (ctx: json, lbl: string, expr: cnl_expr): json =>
  set(ctx, lbl, json_of_expr(expr));

let get_expr = (ctx: json, lbl: string): option(cnl_expr) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | expr =>
    switch (Cnl_t.cnl_expr_of_yojson((expr :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Event} */;

let set_evnt_desc = (ctx: json, lbl: string, desc: cnl_evnt_desc): json =>
  set(ctx, lbl, json_of_evnt_desc(desc));

let get_evnt_desc = (ctx: json, lbl: string): option(cnl_evnt_desc) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | desc =>
    switch (Cnl_t.cnl_evnt_desc_of_yojson((desc :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Cond} */;

let set_cond_desc = (ctx: json, lbl: string, desc: cnl_cond_desc): json =>
  set(ctx, lbl, json_of_cond_desc(desc));

let get_cond_desc = (ctx: json, lbl: string): option(cnl_cond_desc) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | desc =>
    switch (Cnl_t.cnl_cond_desc_of_yojson((desc :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Actions} */;

let set_actns_desc = (ctx: json, lbl: string, desc: cnl_actns_desc): json =>
  set(ctx, lbl, json_of_actns_desc(desc));

let get_actns_desc = (ctx: json, lbl: string): option(cnl_actns_desc) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | desc =>
    switch (Cnl_t.cnl_actns_desc_of_yojson((desc :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Action} */;

let set_actn_desc = (ctx: json, lbl: string, desc: cnl_actn_desc): json =>
  set(ctx, lbl, json_of_actn_desc(desc));

let get_actn_desc = (ctx: json, lbl: string): option(cnl_actn_desc) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | desc =>
    switch (Cnl_t.cnl_actn_desc_of_yojson((desc :> Yojson.Safe.t))) {
    | Deriving_intf.Error(_) => None
    | Deriving_intf.Ok(x) => Some(x)
    }
  };

/** {6. Bool} */;

let set_bool = (ctx: json, lbl: string, b: bool): json =>
  set(ctx, lbl, `Bool(b));

let get_bool = (ctx: json, lbl: string): option(bool) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Bool(b) => Some(b)
  | _ => None
  };

/** {6. String} */;

let set_string = (ctx: json, lbl: string, s: string): json =>
  set(ctx, lbl, `String(s));

let get_string = (ctx: json, lbl: string): option(string) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `String(s) => Some(s)
  | _ => None
  };

let take_string = (ctx: json, lbl: string): (json, option(string)) =>
  switch (take(ctx, lbl)) {
  | (ctx, Some(`String(s))) => (ctx, Some(s))
  | (ctx, _) => (ctx, None)
  };

/** {6. Dispatch} */;

let set_dispatch = (ctx: json, lbl: string, d: dispatch(string)): json =>
  set(
    ctx,
    lbl,
    Yojson.Safe.to_basic(Dialog_util.string_dispatch_to_yojson(d)),
  );

let get_dispatch = (ctx: json, lbl: string): option(dispatch(int)) =>
  switch (Yojson.Basic.Util.member(lbl, ctx)) {
  | `Null => None
  | desc =>
    try(
      switch (Dialog_util.int_dispatch_of_yojson((desc :> Json_t.safe))) {
      | Deriving_intf.Ok(x) => Some(x)
      | Deriving_intf.Error(_) => None
      }
    ) {
    | _ => None
    }
  };

/** {6. Initial context } */

let build_cnl = (kind, focus, prompt) =>
  switch (kind) {
  | K_expr(Some((ename, fname))) =>
    `Assoc([
      ("kind", `String(Cnl_print.cnl_print_kind(kind))),
      ("entity", `String(ename)),
      ("field", `String(fname)),
      ("n", `Int(focus)),
      ("prompt", `String(prompt)),
    ])
  | _ =>
    `Assoc([
      ("kind", `String(Cnl_print.cnl_print_kind(kind))),
      ("entity", `String("")),
      ("field", `String("")),
      ("n", `Int(focus)),
      ("prompt", `String(prompt)),
    ])
  };
