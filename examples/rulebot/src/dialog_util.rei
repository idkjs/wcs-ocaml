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
open Cnl_t;

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

let bypass_expr: string => option((bool, cnl_expr));
let bypass_empty: string => option((bool, 'a));

let match_string: string => option((string, string));

let debug_message: (Wcs_t.message_request, Wcs_t.message_response) => unit;

/** {6 JSON serialization} */

let dispatch_of_yojson:
  (Json_t.safe => Deriving_intf.deriving_error_or('a), Json_t.safe) =>
  Deriving_intf.deriving_result(dispatch('a), string);
let dispatch_to_yojson: ('a => Json_t.safe, dispatch('a)) => Json_t.safe;

let int_dispatch_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(dispatch(int), string);

let string_dispatch_to_yojson: dispatch(string) => Json_t.safe;
