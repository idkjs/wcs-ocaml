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
open Cnl_util;
open Cnl_instr_t;

let cnl_instr_to_string = instr =>
  switch (instr) {
  | [@implicit_arity] I_repl_expr(id, e) => "[REPL EXPR]"
  | [@implicit_arity] I_repl_actn(id, a) => "[REPL ACTN]"
  | [@implicit_arity] I_repl_evnt(id, w) => "[REPL EVNT]"
  | [@implicit_arity] I_repl_cond(id, i) => "[REPL COND]"
  | [@implicit_arity] I_repl_actns(id, t) => "[REPL ACTNS]"
  | [@implicit_arity] I_repl_actns_closed(id, b) =>
    "[REPL_CLOSED " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_expr(id, b) =>
    "[CONF EXPR " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_actn(id, b) =>
    "[CONF ACTN " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_evnt(id, b) =>
    "[CONF EVNT " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_cond(id, b) =>
    "[CONF COND " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_actns(id, b) =>
    "[CONF ACTNS " ++ string_of_bool(b) ++ "]"
  | [@implicit_arity] I_conf_rule(id, b) =>
    "[CONF RULE " ++ string_of_bool(b) ++ "]"
  | I_insr_actn => "[INSR ACTN]"
  };

let notyet = instr =>
  raise(
    Failure(
      "Instruction: " ++ cnl_instr_to_string(instr) ++ " not supported",
    ),
  );

/* Replace instructions */

let replace_desc = (id: int, new_x: 'a, node: node('a)): node('a) =>
  switch (node) {
  | N_undefined(Some(id'))
  | [@implicit_arity] N_filled(Some(id'), _)
  | [@implicit_arity] N_rejected(Some(id'), _) =>
    if (id == id') {
      [@implicit_arity] N_filled(Some(id), new_x);
    } else {
      node;
    }
  | N_undefined(None)
  | [@implicit_arity] N_filled(None, _)
  | [@implicit_arity] N_rejected(None, _)
  | N_accepted(_) => node
  };

let repl_expr = (id: int, new_x, r) =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.expr_map_fun: replace_desc(id, new_x),
    },
    r,
  );

let repl_actn = (id: int, new_x, r) =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.actn_map_fun: replace_desc(id, new_x),
    },
    r,
  );

let repl_evnt = (id: int, new_x, r) =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.evnt_map_fun: replace_desc(id, new_x),
    },
    r,
  );

let repl_cond = (id: int, new_x, r) =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.cond_map_fun: replace_desc(id, new_x),
    },
    r,
  );

let repl_actns = (id: int, new_x, r) =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.actns_map_fun: replace_desc(id, new_x),
    },
    r,
  );

let repl_actns_closed = (id: int, closed, r) => {
  let repl_closed = actns =>
    switch (actns.actns_node) {
    | N_undefined(actns_id)
    | [@implicit_arity] N_rejected(actns_id, _) =>
      let node =
        if (closed) {
          {
            list_elems: [],
            list_closed: [@implicit_arity] N_filled(Some(id), ()),
          };
        } else {
          {
            list_elems: [Cnl_builder.mk_actn_undefined()],
            list_closed: N_undefined(Some(id)),
          };
        };

      {...actns, actns_node: [@implicit_arity] N_filled(actns_id, node)};
    | [@implicit_arity] N_filled(actns_id, desc) =>
      let node =
        if (closed) {
          {
            list_elems: desc.list_elems,
            list_closed: [@implicit_arity] N_filled(Some(id), ()),
          };
        } else {
          {
            list_elems: desc.list_elems @ [Cnl_builder.mk_actn_undefined()],
            list_closed: N_undefined(Some(id)),
          };
        };

      {...actns, actns_node: [@implicit_arity] N_filled(actns_id, node)};
    | N_accepted(_) => assert(false)
    };

  Cnl2cnl.rule_sh_map(evnt => evnt, cond => cond, repl_closed, r);
};

/* Confirm instructions */

let replace = (id, cnl, r) =>
  switch (cnl) {
  | Cnl_expr(expr) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        expr.expr_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.expr_map_fun: map_fun},
      r,
    );
  | Cnl_actn(actn) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        actn.actn_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.actn_map_fun: map_fun},
      r,
    );
  | Cnl_evnt(evnt) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        evnt.evnt_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.evnt_map_fun: map_fun},
      r,
    );
  | Cnl_cond(cond) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        cond.cond_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.cond_map_fun: map_fun},
      r,
    );
  | Cnl_actns(actns) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        actns.actns_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.actns_map_fun: map_fun},
      r,
    );
  | Cnl_rule(rule) =>
    let map_fun = node =>
      if (Some(id) == node_id(node)) {
        rule.rule_node;
      } else {
        node;
      };

    Cnl2cnl.rule_dp_map_over_nodes(
      {...Cnl2cnl.id_map_over_node_fun, Cnl2cnl.rule_map_fun: map_fun},
      r,
    );
  };

let confirm = (id: int, b, r) =>
  switch (rule_get_cnl(Some(id), r)) {
  | Some(cnl) =>
    let cnl =
      if (b) {
        cnl_f_to_a(cnl);
      } else {
        cnl_f_to_r(cnl);
      };

    replace(Some(id), cnl, r);
  | None => r
  };

/* Insert instructions */

let insert_action = r => {
  let r =
    Cnl2cnl.rule_sh_map(
      x => x,
      x => x,
      actns => {
        let node =
          Cnl2cnl.node_sh_map(
            desc =>
              {
                list_elems:
                  desc.list_elems @ [Cnl_builder.mk_actn_undefined()],
                list_closed: desc.list_closed,
              },
            actns.actns_node,
          );

        {...actns, actns_node: node};
      },
      r,
    );

  index_rule(r);
};

/* Main apply */

let cnl_instr_apply = (instr, r: cnl_rule): cnl_rule => {
  let r_applied =
    switch (instr) {
    | [@implicit_arity] I_repl_expr(id, e) => repl_expr(id, e, r)
    | [@implicit_arity] I_repl_actn(id, a) => repl_actn(id, a, r)
    | [@implicit_arity] I_repl_evnt(id, w) => repl_evnt(id, w, r)
    | [@implicit_arity] I_repl_cond(id, c) => repl_cond(id, c, r)
    | [@implicit_arity] I_repl_actns(id, t) => repl_actns(id, t, r)
    | [@implicit_arity] I_repl_actns_closed(id, b) =>
      repl_actns_closed(id, b, r)
    | [@implicit_arity] I_conf_expr(id, b) => confirm(id, b, r)
    | [@implicit_arity] I_conf_actn(id, b) => confirm(id, b, r)
    | [@implicit_arity] I_conf_evnt(id, b) => confirm(id, b, r)
    | [@implicit_arity] I_conf_cond(id, b) => confirm(id, b, r)
    | [@implicit_arity] I_conf_actns(id, b) => confirm(id, b, r)
    | [@implicit_arity] I_conf_rule(id, b) => confirm(id, b, r)
    | I_insr_actn => insert_action(r)
    };
  /* Always re-index the rule to add missing identifiers */

  index_rule(r_applied);
};
