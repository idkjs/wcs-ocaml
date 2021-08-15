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

/** {6. Locations} */;
type location = (Lexing.position, Lexing.position);
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
let default_loc = (Parsing.symbol_start_pos(), Parsing.symbol_end_pos());
/** {6. Nodes}*/;
type id = option(int);
let rec id_to_yojson: id => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | None => `Null
    | Some(x) => (x => `Int(x))(x)
  )
and id_of_yojson: Yojson.Safe.t => error_or(id) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Null => Ok(None)
    | x =>
      (
        fun
        | `Int(x) => Ok(x)
        | _ => Error("Cnl_t.id")
      )(x)
      >>= (x => Ok(Some(x)))
  );
type node('a) =
  | N_undefined(id)
  | N_filled(id, 'a)
  | N_rejected(id, 'a)
  | N_accepted('a);
let rec node_to_yojson: 'a. ('a => Yojson.Safe.t, node('a)) => Yojson.Safe.t =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      fun
      | N_undefined(arg0) =>
        `List([`String("N_undefined"), (x => id_to_yojson(x))(arg0)])
      | [@implicit_arity] N_filled(arg0, arg1) =>
        `List([
          `String("N_filled"),
          (x => id_to_yojson(x))(arg0),
          (poly_a: _ => Yojson.Safe.t)(arg1),
        ])
      | [@implicit_arity] N_rejected(arg0, arg1) =>
        `List([
          `String("N_rejected"),
          (x => id_to_yojson(x))(arg0),
          (poly_a: _ => Yojson.Safe.t)(arg1),
        ])
      | N_accepted(arg0) =>
        `List([`String("N_accepted"), (poly_a: _ => Yojson.Safe.t)(arg0)])
    )
and node_of_yojson:
  'a.
  (Yojson.Safe.t => error_or('a), Yojson.Safe.t) => error_or(node('a))
 =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      fun
      | `List([`String("N_undefined"), arg0]) =>
        (x => id_of_yojson(x))(arg0) >>= (arg0 => Ok(N_undefined(arg0)))
      | `List([`String("N_filled"), arg0, arg1]) =>
        (poly_a: Yojson.Safe.t => error_or(_))(arg1)
        >>= (
          arg1 =>
            (x => id_of_yojson(x))(arg0)
            >>= (arg0 => Ok([@implicit_arity] N_filled(arg0, arg1)))
        )
      | `List([`String("N_rejected"), arg0, arg1]) =>
        (poly_a: Yojson.Safe.t => error_or(_))(arg1)
        >>= (
          arg1 =>
            (x => id_of_yojson(x))(arg0)
            >>= (arg0 => Ok([@implicit_arity] N_rejected(arg0, arg1)))
        )
      | `List([`String("N_accepted"), arg0]) =>
        (poly_a: Yojson.Safe.t => error_or(_))(arg0)
        >>= (arg0 => Ok(N_accepted(arg0)))
      | _ => Error("Cnl_t.node")
    );
type node_list('a) = {
  list_elems: list('a),
  list_closed: node(unit),
};
let rec node_list_to_yojson:
  'a.
  ('a => Yojson.Safe.t, node_list('a)) => Yojson.Safe.t
 =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      x => {
        let fields = [];
        let fields = [
          (
            "list_closed",
            (x => (node_to_yojson(x => `Null))(x))(x.list_closed),
          ),
          ...fields,
        ];
        let fields = [
          (
            "list_elems",
            (x => `List(List.map(poly_a: _ => Yojson.Safe.t, x)))(
              x.list_elems,
            ),
          ),
          ...fields,
        ];
        `Assoc(fields);
      }
    )
and node_list_of_yojson:
  'a.
  (Yojson.Safe.t => error_or('a), Yojson.Safe.t) => error_or(node_list('a))
 =
  poly_a =>
    [@ocaml.warning "-A"]
    (
      fun
      | `Assoc(xs) => {
          let rec loop = (xs, (arg0, arg1) as _state) =>
            switch (xs) {
            | [("list_elems", x), ...xs] =>
              loop(
                xs,
                (
                  (
                    fun
                    | `List(xs) =>
                      map_bind(poly_a: Yojson.Safe.t => error_or(_), [], xs)
                    | _ => Error("Cnl_t.node_list.list_elems")
                  )(
                    x,
                  ),
                  arg1,
                ),
              )
            | [("list_closed", x), ...xs] =>
              loop(
                xs,
                (
                  arg0,
                  (
                    x =>
                      (
                        node_of_yojson(
                          fun
                          | `Null => Ok()
                          | _ => Error("Cnl_t.node_list.list_closed"),
                        )
                      )(
                        x,
                      )
                  )(
                    x,
                  ),
                ),
              )
            | [] =>
              arg1
              >>= (
                arg1 =>
                  arg0 >>= (arg0 => Ok({list_elems: arg0, list_closed: arg1}))
              )
            | [_, ...xs] => Error("Cnl_t.node_list")
            };
          loop(
            xs,
            (
              Error("Cnl_t.node_list.list_elems"),
              Error("Cnl_t.node_list.list_closed"),
            ),
          );
        }
      | _ => Error("Cnl_t.node_list")
    );
/** {6. AST}
    See BNF in ../papers/2017-debs-dialog-odm/cnl_bnf.txt
 */;
type cnl_rule = {
  rule_node: node(cnl_rule_desc),
  rule_loc: [@default default_loc] location,
}
and cnl_rule_desc = {
  rule_evnt: cnl_event,
  rule_cond: cnl_cond,
  rule_actns: cnl_actions,
}
and cnl_event = {
  evnt_node: node(cnl_evnt_desc),
  evnt_loc: [@default default_loc] location,
}
and cnl_evnt_desc = (event_name, option(variable_name))
and cnl_cond = {
  cond_node: node(cnl_cond_desc),
  cond_loc: [@default default_loc] location,
}
and cnl_cond_desc =
  | C_no_condition
  | C_condition(cnl_expr)
and cnl_actions = {
  actns_node: node(cnl_actns_desc),
  actns_loc: [@default default_loc] location,
}
and cnl_actns_desc = node_list(cnl_action)
and cnl_action = {
  actn_node: node(cnl_actn_desc),
  actn_loc: [@default default_loc] location,
}
and cnl_actn_desc =
  | A_print(cnl_expr)
  | A_emit(cnl_expr)
  | A_define(variable_name, cnl_expr)
  | A_set(field_name, variable_name, cnl_expr)
and cnl_expr = {
  expr_node: node(cnl_expr_desc),
  expr_field: option((event_name, field_name)),
  expr_loc: [@default default_loc] location,
}
and cnl_expr_desc =
  | E_lit(cnl_literal)
  | E_var(variable_name)
  | E_get(cnl_expr, field_name)
  | E_agg(cnl_aggop, cnl_expr, field_name)
  | E_unop(cnl_unop, cnl_expr)
  | E_binop(cnl_binop, cnl_expr, cnl_expr)
  | E_error(Yojson.Safe.t)
  | E_this(event_name)
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
let rec cnl_rule_to_yojson: cnl_rule => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.rule_loc == default_loc) {
          fields;
        } else {
          [
            ("rule_loc", (x => location_to_yojson(x))(x.rule_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "rule_node",
          (x => (node_to_yojson(x => cnl_rule_desc_to_yojson(x)))(x))(
            x.rule_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_rule_of_yojson: Yojson.Safe.t => error_or(cnl_rule) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1) as _state) =>
          switch (xs) {
          | [("rule_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_rule_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
              ),
            )
          | [("rule_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x)))
          | [] =>
            arg1
            >>= (
              arg1 => arg0 >>= (arg0 => Ok({rule_node: arg0, rule_loc: arg1}))
            )
          | [_, ...xs] => Error("Cnl_t.cnl_rule")
          };
        loop(xs, (Error("Cnl_t.cnl_rule.rule_node"), Ok(default_loc)));
      }
    | _ => Error("Cnl_t.cnl_rule")
  )
and cnl_rule_desc_to_yojson: cnl_rule_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields = [
        ("rule_actns", (x => cnl_actions_to_yojson(x))(x.rule_actns)),
        ...fields,
      ];
      let fields = [
        ("rule_cond", (x => cnl_cond_to_yojson(x))(x.rule_cond)),
        ...fields,
      ];
      let fields = [
        ("rule_evnt", (x => cnl_event_to_yojson(x))(x.rule_evnt)),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_rule_desc_of_yojson: Yojson.Safe.t => error_or(cnl_rule_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1, arg2) as _state) =>
          switch (xs) {
          | [("rule_evnt", x), ...xs] =>
            loop(xs, ((x => cnl_event_of_yojson(x))(x), arg1, arg2))
          | [("rule_cond", x), ...xs] =>
            loop(xs, (arg0, (x => cnl_cond_of_yojson(x))(x), arg2))
          | [("rule_actns", x), ...xs] =>
            loop(xs, (arg0, arg1, (x => cnl_actions_of_yojson(x))(x)))
          | [] =>
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
                          rule_evnt: arg0,
                          rule_cond: arg1,
                          rule_actns: arg2,
                        })
                    )
                )
            )
          | [_, ...xs] => Error("Cnl_t.cnl_rule_desc")
          };
        loop(
          xs,
          (
            Error("Cnl_t.cnl_rule_desc.rule_evnt"),
            Error("Cnl_t.cnl_rule_desc.rule_cond"),
            Error("Cnl_t.cnl_rule_desc.rule_actns"),
          ),
        );
      }
    | _ => Error("Cnl_t.cnl_rule_desc")
  )
and cnl_event_to_yojson: cnl_event => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.evnt_loc == default_loc) {
          fields;
        } else {
          [
            ("evnt_loc", (x => location_to_yojson(x))(x.evnt_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "evnt_node",
          (x => (node_to_yojson(x => cnl_evnt_desc_to_yojson(x)))(x))(
            x.evnt_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_event_of_yojson: Yojson.Safe.t => error_or(cnl_event) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1) as _state) =>
          switch (xs) {
          | [("evnt_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_evnt_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
              ),
            )
          | [("evnt_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x)))
          | [] =>
            arg1
            >>= (
              arg1 => arg0 >>= (arg0 => Ok({evnt_node: arg0, evnt_loc: arg1}))
            )
          | [_, ...xs] => Error("Cnl_t.cnl_event")
          };
        loop(xs, (Error("Cnl_t.cnl_event.evnt_node"), Ok(default_loc)));
      }
    | _ => Error("Cnl_t.cnl_event")
  )
and cnl_evnt_desc_to_yojson: cnl_evnt_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    ((arg0, arg1)) =>
      `List([
        (x => event_name_to_yojson(x))(arg0),
        (
          fun
          | None => `Null
          | Some(x) => (x => variable_name_to_yojson(x))(x)
        )(
          arg1,
        ),
      ])
  )
and cnl_evnt_desc_of_yojson: Yojson.Safe.t => error_or(cnl_evnt_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([arg0, arg1]) =>
      (
        fun
        | `Null => Ok(None)
        | x => (x => variable_name_of_yojson(x))(x) >>= (x => Ok(Some(x)))
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (x => event_name_of_yojson(x))(arg0)
          >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
      )
    | _ => Error("Cnl_t.cnl_evnt_desc")
  )
and cnl_cond_to_yojson: cnl_cond => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.cond_loc == default_loc) {
          fields;
        } else {
          [
            ("cond_loc", (x => location_to_yojson(x))(x.cond_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "cond_node",
          (x => (node_to_yojson(x => cnl_cond_desc_to_yojson(x)))(x))(
            x.cond_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_cond_of_yojson: Yojson.Safe.t => error_or(cnl_cond) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1) as _state) =>
          switch (xs) {
          | [("cond_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_cond_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
              ),
            )
          | [("cond_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x)))
          | [] =>
            arg1
            >>= (
              arg1 => arg0 >>= (arg0 => Ok({cond_node: arg0, cond_loc: arg1}))
            )
          | [_, ...xs] => Error("Cnl_t.cnl_cond")
          };
        loop(xs, (Error("Cnl_t.cnl_cond.cond_node"), Ok(default_loc)));
      }
    | _ => Error("Cnl_t.cnl_cond")
  )
and cnl_cond_desc_to_yojson: cnl_cond_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | C_no_condition => `List([`String("C_no_condition")])
    | C_condition(arg0) =>
      `List([`String("C_condition"), (x => cnl_expr_to_yojson(x))(arg0)])
  )
and cnl_cond_desc_of_yojson: Yojson.Safe.t => error_or(cnl_cond_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("C_no_condition")]) => Ok(C_no_condition)
    | `List([`String("C_condition"), arg0]) =>
      (x => cnl_expr_of_yojson(x))(arg0)
      >>= (arg0 => Ok(C_condition(arg0)))
    | _ => Error("Cnl_t.cnl_cond_desc")
  )
and cnl_actions_to_yojson: cnl_actions => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.actns_loc == default_loc) {
          fields;
        } else {
          [
            ("actns_loc", (x => location_to_yojson(x))(x.actns_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "actns_node",
          (x => (node_to_yojson(x => cnl_actns_desc_to_yojson(x)))(x))(
            x.actns_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_actions_of_yojson: Yojson.Safe.t => error_or(cnl_actions) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1) as _state) =>
          switch (xs) {
          | [("actns_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_actns_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
              ),
            )
          | [("actns_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x)))
          | [] =>
            arg1
            >>= (
              arg1 =>
                arg0 >>= (arg0 => Ok({actns_node: arg0, actns_loc: arg1}))
            )
          | [_, ...xs] => Error("Cnl_t.cnl_actions")
          };
        loop(xs, (Error("Cnl_t.cnl_actions.actns_node"), Ok(default_loc)));
      }
    | _ => Error("Cnl_t.cnl_actions")
  )
and cnl_actns_desc_to_yojson: cnl_actns_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (x => (node_list_to_yojson(x => cnl_action_to_yojson(x)))(x))
and cnl_actns_desc_of_yojson: Yojson.Safe.t => error_or(cnl_actns_desc) =
  [@ocaml.warning "-A"]
  (x => (node_list_of_yojson(x => cnl_action_of_yojson(x)))(x))
and cnl_action_to_yojson: cnl_action => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.actn_loc == default_loc) {
          fields;
        } else {
          [
            ("actn_loc", (x => location_to_yojson(x))(x.actn_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "actn_node",
          (x => (node_to_yojson(x => cnl_actn_desc_to_yojson(x)))(x))(
            x.actn_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_action_of_yojson: Yojson.Safe.t => error_or(cnl_action) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1) as _state) =>
          switch (xs) {
          | [("actn_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_actn_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
              ),
            )
          | [("actn_loc", x), ...xs] =>
            loop(xs, (arg0, (x => location_of_yojson(x))(x)))
          | [] =>
            arg1
            >>= (
              arg1 => arg0 >>= (arg0 => Ok({actn_node: arg0, actn_loc: arg1}))
            )
          | [_, ...xs] => Error("Cnl_t.cnl_action")
          };
        loop(xs, (Error("Cnl_t.cnl_action.actn_node"), Ok(default_loc)));
      }
    | _ => Error("Cnl_t.cnl_action")
  )
and cnl_actn_desc_to_yojson: cnl_actn_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | A_print(arg0) =>
      `List([`String("A_print"), (x => cnl_expr_to_yojson(x))(arg0)])
    | A_emit(arg0) =>
      `List([`String("A_emit"), (x => cnl_expr_to_yojson(x))(arg0)])
    | [@implicit_arity] A_define(arg0, arg1) =>
      `List([
        `String("A_define"),
        (x => variable_name_to_yojson(x))(arg0),
        (x => cnl_expr_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] A_set(arg0, arg1, arg2) =>
      `List([
        `String("A_set"),
        (x => field_name_to_yojson(x))(arg0),
        (x => variable_name_to_yojson(x))(arg1),
        (x => cnl_expr_to_yojson(x))(arg2),
      ])
  )
and cnl_actn_desc_of_yojson: Yojson.Safe.t => error_or(cnl_actn_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("A_print"), arg0]) =>
      (x => cnl_expr_of_yojson(x))(arg0) >>= (arg0 => Ok(A_print(arg0)))
    | `List([`String("A_emit"), arg0]) =>
      (x => cnl_expr_of_yojson(x))(arg0) >>= (arg0 => Ok(A_emit(arg0)))
    | `List([`String("A_define"), arg0, arg1]) =>
      (x => cnl_expr_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (x => variable_name_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] A_define(arg0, arg1)))
      )
    | `List([`String("A_set"), arg0, arg1, arg2]) =>
      (x => cnl_expr_of_yojson(x))(arg2)
      >>= (
        arg2 =>
          (x => variable_name_of_yojson(x))(arg1)
          >>= (
            arg1 =>
              (x => field_name_of_yojson(x))(arg0)
              >>= (arg0 => Ok([@implicit_arity] A_set(arg0, arg1, arg2)))
          )
      )
    | _ => Error("Cnl_t.cnl_actn_desc")
  )
and cnl_expr_to_yojson: cnl_expr => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    x => {
      let fields = [];
      let fields =
        if (x.expr_loc == default_loc) {
          fields;
        } else {
          [
            ("expr_loc", (x => location_to_yojson(x))(x.expr_loc)),
            ...fields,
          ];
        };
      let fields = [
        (
          "expr_field",
          (
            fun
            | None => `Null
            | Some(x) =>
              (
                ((arg0, arg1)) =>
                  `List([
                    (x => event_name_to_yojson(x))(arg0),
                    (x => field_name_to_yojson(x))(arg1),
                  ])
              )(
                x,
              )
          )(
            x.expr_field,
          ),
        ),
        ...fields,
      ];
      let fields = [
        (
          "expr_node",
          (x => (node_to_yojson(x => cnl_expr_desc_to_yojson(x)))(x))(
            x.expr_node,
          ),
        ),
        ...fields,
      ];
      `Assoc(fields);
    }
  )
and cnl_expr_of_yojson: Yojson.Safe.t => error_or(cnl_expr) =
  [@ocaml.warning "-A"]
  (
    fun
    | `Assoc(xs) => {
        let rec loop = (xs, (arg0, arg1, arg2) as _state) =>
          switch (xs) {
          | [("expr_node", x), ...xs] =>
            loop(
              xs,
              (
                (x => (node_of_yojson(x => cnl_expr_desc_of_yojson(x)))(x))(
                  x,
                ),
                arg1,
                arg2,
              ),
            )
          | [("expr_field", x), ...xs] =>
            loop(
              xs,
              (
                arg0,
                (
                  fun
                  | `Null => Ok(None)
                  | x =>
                    (
                      fun
                      | `List([arg0, arg1]) =>
                        (x => field_name_of_yojson(x))(arg1)
                        >>= (
                          arg1 =>
                            (x => event_name_of_yojson(x))(arg0)
                            >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
                        )
                      | _ => Error("Cnl_t.cnl_expr.expr_field")
                    )(
                      x,
                    )
                    >>= (x => Ok(Some(x)))
                )(
                  x,
                ),
                arg2,
              ),
            )
          | [("expr_loc", x), ...xs] =>
            loop(xs, (arg0, arg1, (x => location_of_yojson(x))(x)))
          | [] =>
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
                          expr_node: arg0,
                          expr_field: arg1,
                          expr_loc: arg2,
                        })
                    )
                )
            )
          | [_, ...xs] => Error("Cnl_t.cnl_expr")
          };
        loop(
          xs,
          (
            Error("Cnl_t.cnl_expr.expr_node"),
            Error("Cnl_t.cnl_expr.expr_field"),
            Ok(default_loc),
          ),
        );
      }
    | _ => Error("Cnl_t.cnl_expr")
  )
and cnl_expr_desc_to_yojson: cnl_expr_desc => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | E_lit(arg0) =>
      `List([`String("E_lit"), (x => cnl_literal_to_yojson(x))(arg0)])
    | E_var(arg0) =>
      `List([`String("E_var"), (x => variable_name_to_yojson(x))(arg0)])
    | [@implicit_arity] E_get(arg0, arg1) =>
      `List([
        `String("E_get"),
        (x => cnl_expr_to_yojson(x))(arg0),
        (x => field_name_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] E_agg(arg0, arg1, arg2) =>
      `List([
        `String("E_agg"),
        (x => cnl_aggop_to_yojson(x))(arg0),
        (x => cnl_expr_to_yojson(x))(arg1),
        (x => field_name_to_yojson(x))(arg2),
      ])
    | [@implicit_arity] E_unop(arg0, arg1) =>
      `List([
        `String("E_unop"),
        (x => cnl_unop_to_yojson(x))(arg0),
        (x => cnl_expr_to_yojson(x))(arg1),
      ])
    | [@implicit_arity] E_binop(arg0, arg1, arg2) =>
      `List([
        `String("E_binop"),
        (x => cnl_binop_to_yojson(x))(arg0),
        (x => cnl_expr_to_yojson(x))(arg1),
        (x => cnl_expr_to_yojson(x))(arg2),
      ])
    | E_error(arg0) => `List([`String("E_error"), (x => x)(arg0)])
    | E_this(arg0) =>
      `List([`String("E_this"), (x => event_name_to_yojson(x))(arg0)])
    | [@implicit_arity] E_new(arg0, arg1) =>
      `List([
        `String("E_new"),
        (x => event_name_to_yojson(x))(arg0),
        (x => `List(List.map(x => cnl_setter_to_yojson(x), x)))(arg1),
      ])
  )
and cnl_expr_desc_of_yojson: Yojson.Safe.t => error_or(cnl_expr_desc) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("E_lit"), arg0]) =>
      (x => cnl_literal_of_yojson(x))(arg0) >>= (arg0 => Ok(E_lit(arg0)))
    | `List([`String("E_var"), arg0]) =>
      (x => variable_name_of_yojson(x))(arg0) >>= (arg0 => Ok(E_var(arg0)))
    | `List([`String("E_get"), arg0, arg1]) =>
      (x => field_name_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (x => cnl_expr_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_get(arg0, arg1)))
      )
    | `List([`String("E_agg"), arg0, arg1, arg2]) =>
      (x => field_name_of_yojson(x))(arg2)
      >>= (
        arg2 =>
          (x => cnl_expr_of_yojson(x))(arg1)
          >>= (
            arg1 =>
              (x => cnl_aggop_of_yojson(x))(arg0)
              >>= (arg0 => Ok([@implicit_arity] E_agg(arg0, arg1, arg2)))
          )
      )
    | `List([`String("E_unop"), arg0, arg1]) =>
      (x => cnl_expr_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (x => cnl_unop_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_unop(arg0, arg1)))
      )
    | `List([`String("E_binop"), arg0, arg1, arg2]) =>
      (x => cnl_expr_of_yojson(x))(arg2)
      >>= (
        arg2 =>
          (x => cnl_expr_of_yojson(x))(arg1)
          >>= (
            arg1 =>
              (x => cnl_binop_of_yojson(x))(arg0)
              >>= (arg0 => Ok([@implicit_arity] E_binop(arg0, arg1, arg2)))
          )
      )
    | `List([`String("E_error"), arg0]) =>
      (x => Ok(x))(arg0) >>= (arg0 => Ok(E_error(arg0)))
    | `List([`String("E_this"), arg0]) =>
      (x => event_name_of_yojson(x))(arg0) >>= (arg0 => Ok(E_this(arg0)))
    | `List([`String("E_new"), arg0, arg1]) =>
      (
        fun
        | `List(xs) => map_bind(x => cnl_setter_of_yojson(x), [], xs)
        | _ => Error("Cnl_t.cnl_expr_desc")
      )(
        arg1,
      )
      >>= (
        arg1 =>
          (x => event_name_of_yojson(x))(arg0)
          >>= (arg0 => Ok([@implicit_arity] E_new(arg0, arg1)))
      )
    | _ => Error("Cnl_t.cnl_expr_desc")
  )
and cnl_setter_to_yojson: cnl_setter => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    ((arg0, arg1)) =>
      `List([
        (x => field_name_to_yojson(x))(arg0),
        (x => cnl_expr_to_yojson(x))(arg1),
      ])
  )
and cnl_setter_of_yojson: Yojson.Safe.t => error_or(cnl_setter) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([arg0, arg1]) =>
      (x => cnl_expr_of_yojson(x))(arg1)
      >>= (
        arg1 =>
          (x => field_name_of_yojson(x))(arg0)
          >>= (arg0 => [@implicit_arity] Ok(arg0, arg1))
      )
    | _ => Error("Cnl_t.cnl_setter")
  )
and cnl_literal_to_yojson: cnl_literal => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | L_string(arg0) =>
      `List([`String("L_string"), (x => `String(x))(arg0)])
    | L_int(arg0) => `List([`String("L_int"), (x => `Int(x))(arg0)])
    | L_int_as_string(arg0) =>
      `List([`String("L_int_as_string"), (x => `String(x))(arg0)])
    | L_real(arg0) => `List([`String("L_real"), (x => `Float(x))(arg0)])
    | L_real_as_string(arg0) =>
      `List([`String("L_real_as_string"), (x => `String(x))(arg0)])
    | L_boolean(arg0) =>
      `List([`String("L_boolean"), (x => `Bool(x))(arg0)])
    | L_boolean_as_string(arg0) =>
      `List([`String("L_boolean_as_string"), (x => `String(x))(arg0)])
    | L_enum(arg0) => `List([`String("L_enum"), (x => `String(x))(arg0)])
    | L_date(arg0) => `List([`String("L_date"), (x => `String(x))(arg0)])
    | L_duration(arg0) =>
      `List([`String("L_duration"), (x => `String(x))(arg0)])
  )
and cnl_literal_of_yojson: Yojson.Safe.t => error_or(cnl_literal) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("L_string"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_string(arg0)))
    | `List([`String("L_int"), arg0]) =>
      (
        fun
        | `Int(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_int(arg0)))
    | `List([`String("L_int_as_string"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_int_as_string(arg0)))
    | `List([`String("L_real"), arg0]) =>
      (
        fun
        | `Int(x) => Ok(float_of_int(x))
        | `Intlit(x) => Ok(float_of_string(x))
        | `Float(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(
        arg0,
      )
      >>= (arg0 => Ok(L_real(arg0)))
    | `List([`String("L_real_as_string"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_real_as_string(arg0)))
    | `List([`String("L_boolean"), arg0]) =>
      (
        fun
        | `Bool(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_boolean(arg0)))
    | `List([`String("L_boolean_as_string"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_boolean_as_string(arg0)))
    | `List([`String("L_enum"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_enum(arg0)))
    | `List([`String("L_date"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_date(arg0)))
    | `List([`String("L_duration"), arg0]) =>
      (
        fun
        | `String(x) => Ok(x)
        | _ => Error("Cnl_t.cnl_literal")
      )(arg0)
      >>= (arg0 => Ok(L_duration(arg0)))
    | _ => Error("Cnl_t.cnl_literal")
  )
and event_name_to_yojson: event_name => Yojson.Safe.t =
  [@ocaml.warning "-A"] (x => `String(x))
and event_name_of_yojson: Yojson.Safe.t => error_or(event_name) =
  [@ocaml.warning "-A"]
  (
    fun
    | `String(x) => Ok(x)
    | _ => Error("Cnl_t.event_name")
  )
and variable_name_to_yojson: variable_name => Yojson.Safe.t =
  [@ocaml.warning "-A"] (x => `String(x))
and variable_name_of_yojson: Yojson.Safe.t => error_or(variable_name) =
  [@ocaml.warning "-A"]
  (
    fun
    | `String(x) => Ok(x)
    | _ => Error("Cnl_t.variable_name")
  )
and field_name_to_yojson: field_name => Yojson.Safe.t =
  [@ocaml.warning "-A"] (x => `String(x))
and field_name_of_yojson: Yojson.Safe.t => error_or(field_name) =
  [@ocaml.warning "-A"]
  (
    fun
    | `String(x) => Ok(x)
    | _ => Error("Cnl_t.field_name")
  )
and cnl_unop_to_yojson: cnl_unop => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | Op_not => `List([`String("Op_not")])
    | Op_toString => `List([`String("Op_toString")])
  )
and cnl_unop_of_yojson: Yojson.Safe.t => error_or(cnl_unop) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("Op_not")]) => Ok(Op_not)
    | `List([`String("Op_toString")]) => Ok(Op_toString)
    | _ => Error("Cnl_t.cnl_unop")
  )
and cnl_binop_to_yojson: cnl_binop => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | Op_eq => `List([`String("Op_eq")])
    | Op_ne => `List([`String("Op_ne")])
    | Op_lt => `List([`String("Op_lt")])
    | Op_le => `List([`String("Op_le")])
    | Op_gt => `List([`String("Op_gt")])
    | Op_ge => `List([`String("Op_ge")])
    | Op_and => `List([`String("Op_and")])
    | Op_or => `List([`String("Op_or")])
    | Op_plus => `List([`String("Op_plus")])
    | Op_minus => `List([`String("Op_minus")])
    | Op_mult => `List([`String("Op_mult")])
    | Op_div => `List([`String("Op_div")])
    | Op_mod => `List([`String("Op_mod")])
    | Op_pow => `List([`String("Op_pow")])
    | Op_concat => `List([`String("Op_concat")])
    | Op_during => `List([`String("Op_during")])
  )
and cnl_binop_of_yojson: Yojson.Safe.t => error_or(cnl_binop) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("Op_eq")]) => Ok(Op_eq)
    | `List([`String("Op_ne")]) => Ok(Op_ne)
    | `List([`String("Op_lt")]) => Ok(Op_lt)
    | `List([`String("Op_le")]) => Ok(Op_le)
    | `List([`String("Op_gt")]) => Ok(Op_gt)
    | `List([`String("Op_ge")]) => Ok(Op_ge)
    | `List([`String("Op_and")]) => Ok(Op_and)
    | `List([`String("Op_or")]) => Ok(Op_or)
    | `List([`String("Op_plus")]) => Ok(Op_plus)
    | `List([`String("Op_minus")]) => Ok(Op_minus)
    | `List([`String("Op_mult")]) => Ok(Op_mult)
    | `List([`String("Op_div")]) => Ok(Op_div)
    | `List([`String("Op_mod")]) => Ok(Op_mod)
    | `List([`String("Op_pow")]) => Ok(Op_pow)
    | `List([`String("Op_concat")]) => Ok(Op_concat)
    | `List([`String("Op_during")]) => Ok(Op_during)
    | _ => Error("Cnl_t.cnl_binop")
  )
and cnl_aggop_to_yojson: cnl_aggop => Yojson.Safe.t =
  [@ocaml.warning "-A"]
  (
    fun
    | A_total => `List([`String("A_total")])
    | A_avg => `List([`String("A_avg")])
  )
and cnl_aggop_of_yojson: Yojson.Safe.t => error_or(cnl_aggop) =
  [@ocaml.warning "-A"]
  (
    fun
    | `List([`String("A_total")]) => Ok(A_total)
    | `List([`String("A_avg")]) => Ok(A_avg)
    | _ => Error("Cnl_t.cnl_aggop")
  );
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
