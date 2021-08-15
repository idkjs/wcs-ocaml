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

/** {6. Printer util} */;

let string_of_cnl_agg: cnl_aggop => string;

let string_of_cnl_unop: cnl_unop => string;

let string_of_cnl_binop: cnl_binop => string;

/** {6. Access} */;

let node_id: node('a) => option(id);

let node_desc: node('a) => option('a);

let rule_find_node_kind: (id, cnl_rule) => option(cnl_kind);

/** {6. Collect Nodes} */;

/** {8. Undefined} */;

let rule_get_undefined: cnl_rule => list((id, cnl_kind));

let evnt_get_undefined: cnl_event => list((id, cnl_kind));

let cond_get_undefined: cnl_cond => list((id, cnl_kind));

let actns_get_undefined: cnl_actions => list((id, cnl_kind));

let actn_get_undefined: cnl_action => list((id, cnl_kind));

let expr_get_undefined: cnl_expr => list((id, cnl_kind));

/** {8. Filled} */;

let rule_get_filled: cnl_rule => list((id, cnl_kind));

let evnt_get_filled: cnl_event => list((id, cnl_kind));

let cond_get_filled: cnl_cond => list((id, cnl_kind));

let actns_get_filled: cnl_actions => list((id, cnl_kind));

let actn_get_filled: cnl_action => list((id, cnl_kind));

let expr_get_filled: cnl_expr => list((id, cnl_kind));

/** {8. Rejected} */;

let rule_get_rejected: cnl_rule => list((id, cnl_kind));

let evnt_get_rejected: cnl_event => list((id, cnl_kind));

let cond_get_rejected: cnl_cond => list((id, cnl_kind));

let actns_get_rejected: cnl_actions => list((id, cnl_kind));

let actn_get_rejected: cnl_action => list((id, cnl_kind));

let expr_get_rejected: cnl_expr => list((id, cnl_kind));

/** {6. Find focus} */;

let rule_next_focus: (int, cnl_rule) => option((int, cnl_kind));

let cond_next_focus: (int, cnl_rule) => option((int, cnl_kind));

/** {6. Get subtree} */;

let expr_get_cnl: (id, cnl_expr) => option(cnl_ast);

let evnt_get_cnl: (id, cnl_event) => option(cnl_ast);

let cond_get_cnl: (id, cnl_cond) => option(cnl_ast);

let actn_get_cnl: (id, cnl_action) => option(cnl_ast);

let actns_get_cnl: (id, cnl_actions) => option(cnl_ast);

let rule_get_cnl: (id, cnl_rule) => option(cnl_ast);

/** {6. Renaming } */;

let index_rule: cnl_rule => cnl_rule;

/** {6. Change Node State} */;

/** {8. Filled to Accepted} */;

let f_to_a: node('a) => node('a);

let expr_f_to_a: cnl_expr => cnl_expr;

let actn_f_to_a: cnl_action => cnl_action;

let evnt_f_to_a: cnl_event => cnl_event;

let cond_f_to_a: cnl_cond => cnl_cond;

let actns_f_to_a: cnl_actions => cnl_actions;

let rule_f_to_a: cnl_rule => cnl_rule;

let cnl_f_to_a: cnl_ast => cnl_ast;

/** {8. Filled to Reject} */;

let f_to_r: node('a) => node('a);

let expr_f_to_r: cnl_expr => cnl_expr;

let actn_f_to_r: cnl_action => cnl_action;

let evnt_f_to_r: cnl_event => cnl_event;

let cond_f_to_r: cnl_cond => cnl_cond;

let actns_f_to_r: cnl_actions => cnl_actions;

let rule_f_to_r: cnl_rule => cnl_rule;

let cnl_f_to_r: cnl_ast => cnl_ast;

/** {6. modify tree } */;

let add_cond: cnl_rule => cnl_rule;

/** {6. prompt message} */;

let expr_prompt: (id, cnl_expr) => string;
let rule_prompt: (id, cnl_rule) => option(string);

/** {6. Conversion to Yojson.Basic} */;

let json_of_expr: cnl_expr => Json_t.basic;
let json_of_expr_desc: cnl_expr_desc => Json_t.basic;
let json_of_evnt: cnl_event => Json_t.basic;
let json_of_evnt_desc: cnl_evnt_desc => Json_t.basic;
let json_of_cond: cnl_cond => Json_t.basic;
let json_of_cond_desc: cnl_cond_desc => Json_t.basic;
let json_of_actn: cnl_action => Json_t.basic;
let json_of_actn_desc: cnl_actn_desc => Json_t.basic;
let json_of_actns: cnl_actions => Json_t.basic;
let json_of_actns_desc: cnl_actns_desc => Json_t.basic;
let json_of_rule: cnl_rule => Json_t.basic;
let json_of_rule_desc: cnl_rule_desc => Json_t.basic;
let json_replace: (string, string, Json_t.basic) => Json_t.basic;
