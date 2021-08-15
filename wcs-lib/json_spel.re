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

open Wcs_t;

type json_spel = Json_spel_t.json_spel;

/** {6 Builders} */;

let null: json_spel = (`Null: json_spel);

let int = (n: int): json_spel => `Int(n);

let bool = (b: bool): json_spel => `Bool(b);

let string = (s: string): json_spel => `Expr(Spel.string(s));

let assoc = (o: list((string, json_spel))): json_spel => `Assoc(o);

let list = (l: list(json_spel)): json_spel => `List(l);

/** {6 [json]/[json_spel] conversion} */;

let rec to_json = (j: json_spel): json => Json_spel_t.yojson_of_json_spel(j);

let rec of_json = (j: json): json_spel => Json_spel_t.json_spel_of_yojson(j);

/** {6 Manipulation functions} */;

let set = (ctx: json_spel, lbl: string, v: json_spel): json_spel =>
  switch (ctx) {
  | `Null => `Assoc([(lbl, v)])
  | `Assoc(l) => `Assoc([(lbl, v), ...List.remove_assoc(lbl, l)])
  | _ =>
    Log.error(
      "Json_spel",
      Some(ctx),
      "Unable to add a property to a non-object value",
    )
  };

let take = (ctx: json_spel, lbl: string): (json_spel, option(json_spel)) =>
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

let get = (ctx: json_spel, lbl: string): option(json_spel) =>
  switch (ctx) {
  | `Assoc(l) =>
    try(Some(List.assoc(lbl, l))) {
    | Not_found => None
    }
  | _ => None
  };

let assign = (os: list(json_spel)): json_spel =>
  List.fold_left(
    (acc, o) =>
      switch (o) {
      | `Assoc(l) =>
        List.fold_right(((lbl, v), acc) => set(acc, lbl, v), l, acc)
      | _ => Log.error("Json", Some(acc), "")
      },
    null,
    os,
  );

let push = (ctx: json_spel, lbl: string, v: json_spel): json_spel =>
  switch (take(ctx, lbl)) {
  | (ctx, None | Some(`Null)) => set(ctx, lbl, `List([v]))
  | (ctx, Some(`List(l))) => set(ctx, lbl, `List(l @ [v]))
  | (ctc, Some(_)) =>
    Log.error(
      "Json",
      Some(ctx),
      "Unable to push an element in a non-list property",
    )
  };

let pop = (ctx: json_spel, lbl: string): (json_spel, option(json_spel)) =>
  switch (take(ctx, lbl)) {
  | (ctx, Some(`List([v, ...l]))) => (set(ctx, lbl, `List(l)), Some(v))
  | _ => (ctx, None)
  };

/** {6 Settes and getters} */;

/** {8 Bool} */;

let set_bool = (ctx: json_spel, lbl: string, b: bool): json_spel =>
  set(ctx, lbl, `Bool(b));

let get_bool = (ctx: json_spel, lbl: string): option(bool) =>
  switch (get(ctx, lbl)) {
  | Some(`Bool(b)) => Some(b)
  | _ => None
  };

/** {8 String} */;

let set_string = (ctx: json_spel, lbl: string, s: string): json_spel =>
  set(ctx, lbl, `Expr(Spel.string(s)));

/* let get_string (ctx: json_spel) (lbl: string) : string option =
   begin match get ctx lbl with
   | Some (`String s) -> Some s
   | _ -> None
   end */

/* let take_string (ctx: json_spel) (lbl: string) : json_spel * string option =
   begin match take ctx lbl with
   | ctx, Some (`String s) -> ctx, Some s
   | ctx, _ -> ctx, None
   end */
