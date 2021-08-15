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

type workspace_ids = {
  ws_dispatch_id: string,
  ws_when_id: string,
  ws_cond_id: string,
  ws_cond_continue_id: string,
  ws_then_id: string,
  ws_expr_id: string,
  ws_actn_id: string,
  ws_accept_id: string,
};
type dispatch('a) = {
  dsp_replace: bool,
  dsp_abort: bool,
  dsp_number: option('a),
  dsp_when: bool,
  dsp_cond: bool,
  dsp_then: bool,
};
let rec dispatch_to_yojson:
  'a.
  ('a => Yojson.Safe.t, dispatch('a)) => Yojson.Safe.t
 =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      x => {
        let fields = [];
        let fields = [
          ("dsp_then", (x => `Bool(x))(x.dsp_then)),
          ...fields,
        ];
        let fields = [
          ("dsp_cond", (x => `Bool(x))(x.dsp_cond)),
          ...fields,
        ];
        let fields = [
          ("dsp_when", (x => `Bool(x))(x.dsp_when)),
          ...fields,
        ];
        let fields = [
          (
            "dsp_number",
            (
              fun
              | None => `Null
              | Some(x) => (poly_a: _ => Yojson.Safe.t)(x)
            )(
              x.dsp_number,
            ),
          ),
          ...fields,
        ];
        let fields = [
          ("dsp_abort", (x => `Bool(x))(x.dsp_abort)),
          ...fields,
        ];
        let fields = [
          ("dsp_replace", (x => `Bool(x))(x.dsp_replace)),
          ...fields,
        ];
        `Assoc(fields);
      }
    )
and dispatch_of_yojson:
  'a.
  (Yojson.Safe.t => error_or('a), Yojson.Safe.t) => error_or(dispatch('a))
 =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      fun
      | `Assoc(xs) => {
          let rec loop = (xs, (arg0, arg1, arg2, arg3, arg4, arg5) as _state) =>
            switch (xs) {
            | [("dsp_replace", x), ...xs] =>
              loop(
                xs,
                (
                  (
                    fun
                    | `Bool(x) => Ok(x)
                    | _ => Error("Dialog_util.dispatch.dsp_replace")
                  )(
                    x,
                  ),
                  arg1,
                  arg2,
                  arg3,
                  arg4,
                  arg5,
                ),
              )
            | [("dsp_abort", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  (
                    fun
                    | `Bool(x) => Ok(x)
                    | _ => Error("Dialog_util.dispatch.dsp_abort")
                  )(
                    x,
                  ),
                  arg2,
                  arg3,
                  arg4,
                  arg5,
                ),
              )
            | [("dsp_number", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  arg1,
                  (
                    fun
                    | `Null => Ok(None)
                    | x =>
                      (poly_a: Yojson.Safe.t => error_or(_))(x)
                      >>= (x => Ok(Some(x)))
                  )(
                    x,
                  ),
                  arg3,
                  arg4,
                  arg5,
                ),
              )
            | [("dsp_when", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  arg1,
                  arg2,
                  (
                    fun
                    | `Bool(x) => Ok(x)
                    | _ => Error("Dialog_util.dispatch.dsp_when")
                  )(
                    x,
                  ),
                  arg4,
                  arg5,
                ),
              )
            | [("dsp_cond", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  arg1,
                  arg2,
                  arg3,
                  (
                    fun
                    | `Bool(x) => Ok(x)
                    | _ => Error("Dialog_util.dispatch.dsp_cond")
                  )(
                    x,
                  ),
                  arg5,
                ),
              )
            | [("dsp_then", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  arg1,
                  arg2,
                  arg3,
                  arg4,
                  (
                    fun
                    | `Bool(x) => Ok(x)
                    | _ => Error("Dialog_util.dispatch.dsp_then")
                  )(
                    x,
                  ),
                ),
              )
            | [] =>
              arg5
              >>= (
                arg5 =>
                  arg4
                  >>= (
                    arg4 =>
                      arg3
                      >>= (
                        arg3 =>
                          arg2
                          >>= (
                            arg2 =>
                              arg1
                              >>= (
                                arg1 =>
                                  arg0
                                  >>= (
                                    arg0 =>
                                      Ok({
                                        dsp_replace: arg0,
                                        dsp_abort: arg1,
                                        dsp_number: arg2,
                                        dsp_when: arg3,
                                        dsp_cond: arg4,
                                        dsp_then: arg5,
                                      })
                                  )
                              )
                          )
                      )
                  )
              )
            | [_, ...xs] => Error("Dialog_util.dispatch")
            };
          loop(
            xs,
            (
              Error("Dialog_util.dispatch.dsp_replace"),
              Error("Dialog_util.dispatch.dsp_abort"),
              Error("Dialog_util.dispatch.dsp_number"),
              Error("Dialog_util.dispatch.dsp_when"),
              Error("Dialog_util.dispatch.dsp_cond"),
              Error("Dialog_util.dispatch.dsp_then"),
            ),
          );
        }
      | _ => Error("Dialog_util.dispatch")
    );
let int_dispatch_of_yojson =
  dispatch_of_yojson(json =>
    switch (json) {
    | `Int(n) => Ok(n)
    | _ =>
      Error("int_dispatch_of_yojson: " ++ Yojson.Safe.pretty_to_string(json))
    }
  );
let string_dispatch_to_yojson = dispatch_to_yojson(x => `String(x));
let bypass_expr = input =>
  try({
    let regexp = Str.regexp("`\\(.*\\)`");
    let _ = Str.search_forward(regexp, input, 0);
    let quoted_string = Str.matched_group(1, input);
    try(Some((false, Parser_util.parse_cnl_expr_from_string(quoted_string)))) {
    | e =>
      Io_util.print_berl_error(quoted_string);
      None;
    };
  }) {
  | Not_found => None
  };
let bypass_empty = input => None;
/** */;
/** */
let match_string = s =>
  try(
    try({
      let regexp = Str.regexp("\"\\(.*\\)\"");
      let _ = Str.search_forward(regexp, s, 0);
      let string = Str.matched_group(1, s);
      let s_minus_string = Str.global_replace(regexp, "", s);
      Some((s_minus_string, string));
    }) {
    | Not_found =>
      let regexp = Str.regexp("'\\(.*\\)'");
      let _ = Str.search_forward(regexp, s, 0);
      let string = Str.matched_group(1, s);
      let s_minus_string = Str.global_replace(regexp, "", s);
      Some((s_minus_string, string));
    }
  ) {
  | Not_found => None
  };
/** Debug functions */
let debug_message = (req, resp) => {
  Format.eprintf("request:@\n%s@\n", Wcs_j.string_of_message_request(req));
  Format.eprintf("response:@\n%s@.", Wcs_j.string_of_message_response(resp));
};
