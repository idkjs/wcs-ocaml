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

open Cnl_t;

let set_rule_init: cnl_rule => unit;
let rule_init: unit => cnl_rule;
let empty_init: unit => cnl_rule;
let cond_init: unit => cnl_cond_desc;
let actns_init: unit => cnl_actns_desc;
let print_init: unit => cnl_actn_desc;
let emit_init: unit => cnl_actn_desc;
let define_init: variable_name => cnl_actn_desc;
let set_init: (field_name, variable_name) => cnl_actn_desc;
let define1: cnl_action;
let emit1: cnl_action;
let when1: (string, option(string));
let cond1: cnl_cond_desc;
let then1: node_list(cnl_action);
let rule1: cnl_rule;
let define21: cnl_action;
let define22: cnl_action;
let setdesc21: cnl_actn_desc;
let setdesc22: cnl_actn_desc;
let set21: cnl_action;
let set22: cnl_action;
let rule2: cnl_rule;
let cnl_samples: list((string, cnl_rule));
let expr1: cnl_expr;
let expr2: cnl_expr;
