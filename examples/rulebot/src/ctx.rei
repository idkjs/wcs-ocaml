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
open Call_t;

let null: json;

let set: (json, string, json) => json;
let take: (json, string) => (json, option(json));
let get: (json, string) => option(json);

let set_skip_user_input: (json, bool) => json;
let take_skip_user_input: json => (json, bool);

let set_call: (json, call) => json;
let take_call: json => (json, option(call));

let set_return: (json, json) => json;
let get_return: json => option(json);

let set_rule: (json, string, cnl_rule) => json;
let get_rule: (json, string) => option(cnl_rule);

let set_expr: (json, string, cnl_expr) => json;
let get_expr: (json, string) => option(cnl_expr);

let set_evnt_desc: (json, string, cnl_evnt_desc) => json;
let get_evnt_desc: (json, string) => option(cnl_evnt_desc);

let set_cond_desc: (json, string, cnl_cond_desc) => json;
let get_cond_desc: (json, string) => option(cnl_cond_desc);

let set_actns_desc: (json, string, cnl_actns_desc) => json;
let get_actns_desc: (json, string) => option(cnl_actns_desc);

let set_actn_desc: (json, string, cnl_actn_desc) => json;
let get_actn_desc: (json, string) => option(cnl_actn_desc);

let set_bool: (json, string, bool) => json;
let get_bool: (json, string) => option(bool);

let set_string: (json, string, string) => json;
let get_string: (json, string) => option(string);
let take_string: (json, string) => (json, option(string));

let set_dispatch: (json, string, Dialog_util.dispatch(string)) => json;
let get_dispatch: (json, string) => option(Dialog_util.dispatch(int));

let build_cnl: (cnl_kind, int, string) => json;
