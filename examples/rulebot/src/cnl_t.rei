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

/* type 'a error_or = ('a, string) result */

/** {6. Locations} */;

type location = (Lexing.position, Lexing.position);

/** {6. Nodes}*/;

type id = option(int);

type node('a) =
  | N_undefined(id)
  | N_filled(id, 'a)
  | N_rejected(id, 'a)
  | N_accepted('a);

type node_list('a) = {
  list_elems: list('a),
  list_closed: node(unit),
};

/** {6. AST}
    See BNF in ../papers/2017-debs-dialog-odm/cnl_bnf.txt
*/;

type cnl_rule = {
  rule_node: node(cnl_rule_desc),
  rule_loc: location,
}
and cnl_rule_desc = {
  rule_evnt: cnl_event,
  rule_cond: cnl_cond,
  rule_actns: cnl_actions,
}

and cnl_event = {
  evnt_node: node(cnl_evnt_desc),
  evnt_loc: location,
}
and cnl_evnt_desc = (event_name, option(variable_name))

and cnl_cond = {
  cond_node: node(cnl_cond_desc),
  cond_loc: location,
}
and cnl_cond_desc =
  | C_no_condition
  | C_condition(cnl_expr)

and cnl_actions = {
  actns_node: node(cnl_actns_desc),
  actns_loc: location,
}
and cnl_actns_desc = node_list(cnl_action)

and cnl_action = {
  actn_node: node(cnl_actn_desc),
  actn_loc: location,
}
and cnl_actn_desc =
  | A_print(cnl_expr)
  | A_emit(cnl_expr)
  | A_define(variable_name, cnl_expr)
  | A_set(field_name, variable_name, cnl_expr)

and cnl_expr = {
  expr_node: node(cnl_expr_desc),
  expr_field: option((event_name, field_name)), /* XXX Hack? -- contextual information for fields within new XXX */
  expr_loc: location,
}
and cnl_expr_desc =
  | E_lit(cnl_literal)
  | E_var(variable_name)
  | E_get(cnl_expr, field_name)
  | E_agg(cnl_aggop, cnl_expr, field_name) /* XXX TODO: review */
  | E_unop(cnl_unop, cnl_expr)
  | E_binop(cnl_binop, cnl_expr, cnl_expr)
  | E_error(Json_t.safe)
  | E_this(string) /* for current object */
  | E_new(event_name, list(cnl_setter))
and cnl_setter = (field_name, cnl_expr)

and cnl_literal =
  | L_string(string)
  | L_int(int)
  | L_int_as_string(string)
  | L_real(float)
  | L_real_as_string(string)
  | L_boolean(bool)
  | L_boolean_as_string(string)
  | L_enum(string)
  | L_date(string)
  | L_duration(string)

and event_name = string
and variable_name = string
and field_name = string

and cnl_unop =
  | Op_not
  | Op_toString

and cnl_binop =
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
  | Op_plus
  | Op_minus
  | Op_mult
  | Op_div
  | Op_mod
  | Op_pow
  | Op_concat
  | Op_during

and cnl_aggop =
  | A_total
  | A_avg;

/** {6. CNL}*/;

type cnl_kind =
  | K_expr(option((event_name, field_name)))
  | K_actn
  | K_evnt
  | K_cond
  | K_actns
  | K_actns_closed
  | K_rule;

type cnl_ast =
  | Cnl_expr(cnl_expr)
  | Cnl_actn(cnl_action)
  | Cnl_evnt(cnl_event)
  | Cnl_cond(cnl_cond)
  | Cnl_actns(cnl_actions)
  | Cnl_rule(cnl_rule);

/** {6 JSON serialization} */

let cnl_expr_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_expr_desc, string);
let cnl_expr_desc_to_yojson: cnl_expr_desc => Json_t.safe;

let cnl_expr_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_expr, string);
let cnl_expr_to_yojson: cnl_expr => Json_t.safe;

let cnl_actn_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_actn_desc, string);
let cnl_actn_desc_to_yojson: cnl_actn_desc => Json_t.safe;

let cnl_action_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_action, string);
let cnl_action_to_yojson: cnl_action => Json_t.safe;

let cnl_evnt_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_evnt_desc, string);
let cnl_evnt_desc_to_yojson: cnl_evnt_desc => Json_t.safe;

let cnl_event_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_event, string);
let cnl_event_to_yojson: cnl_event => Json_t.safe;

let cnl_cond_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_cond_desc, string);
let cnl_cond_desc_to_yojson: cnl_cond_desc => Json_t.safe;

let cnl_cond_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_cond, string);
let cnl_cond_to_yojson: cnl_cond => Json_t.safe;

let cnl_actns_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_actns_desc, string);
let cnl_actns_desc_to_yojson: cnl_actns_desc => Json_t.safe;

let cnl_actions_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_actions, string);
let cnl_actions_to_yojson: cnl_actions => Json_t.safe;

let cnl_rule_desc_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_rule_desc, string);
let cnl_rule_desc_to_yojson: cnl_rule_desc => Json_t.safe;

let cnl_rule_of_yojson:
  Json_t.safe => Deriving_intf.deriving_result(cnl_rule, string);
let cnl_rule_to_yojson: cnl_rule => Json_t.safe;
