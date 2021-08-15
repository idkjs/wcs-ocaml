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

let string_of_cnl_agg = agg =>
  switch (agg) {
  | A_total => "total"
  | A_avg => "average"
  };

let string_of_cnl_unop = op =>
  switch (op) {
  | Op_not => "not"
  | Op_toString => "toString"
  };

let string_of_cnl_binop = op =>
  switch (op) {
  | Op_eq => "is"
  | Op_ne => "is not"
  | Op_lt => "is less than"
  | Op_le => "is under"
  | Op_gt => "is more than"
  | Op_ge => "is over"
  | Op_and => "and"
  | Op_or => "or"
  | Op_plus => "+"
  | Op_minus => "-"
  | Op_mult => "*"
  | Op_div => "/"
  | Op_mod => "%"
  | Op_pow => "^"
  | Op_concat => "+"
  | Op_during => "during"
  };

/** {6. Access} */;

let node_desc = node =>
  switch (node) {
  | N_undefined(_) => None
  | [@implicit_arity] N_filled(_, desc)
  | [@implicit_arity] N_rejected(_, desc)
  | N_accepted(desc) => Some(desc)
  };

let node_id = node =>
  switch (node) {
  | N_undefined(id)
  | [@implicit_arity] N_filled(id, _)
  | [@implicit_arity] N_rejected(id, _) => Some(id)
  | N_accepted(_) => None
  };

/** {6. Find nodes} */;

let find_node_kind =
    (id: id, kind: cnl_kind, node: node('a), acc: option(cnl_kind))
    : option(cnl_kind) =>
  switch (node) {
  | N_undefined(id')
  | [@implicit_arity] N_filled(id', _)
  | [@implicit_arity] N_rejected(id', _) =>
    if (id == id') {
      switch (acc) {
      | None => Some(kind)
      | Some(_) => acc
      };
    } else {
      acc;
    }
  | N_accepted(_) => acc
  };

let find_node_kind_fold_fun = (id: id) => {
  let f = (kind, node, acc) => find_node_kind(id, kind, node, acc);
  {...Cnl2cnl.id_fold_over_node_fun, Cnl2cnl.poly_fold_fun: f};
};

let rule_find_node_kind = (id: id, rule: cnl_rule): option(cnl_kind) =>
  Cnl2cnl.rule_dp_fold_over_nodes(find_node_kind_fold_fun(id), rule, None);

/** {6. Collect Nodes} */;

/** {8. Node ids} */;

let get_node_ids =
    (
      kind: cnl_kind,
      node: node('a),
      acc: list((id, cnl_kind, node(unit))),
    )
    : list((id, cnl_kind, node(unit))) =>
  switch (node) {
  | N_undefined(id) => [(id, kind, N_undefined(id)), ...acc]
  | [@implicit_arity] N_filled(id, _) => [
      (id, kind, [@implicit_arity] N_filled(id, ())),
      ...acc,
    ]
  | [@implicit_arity] N_rejected(id, _) => [
      (id, kind, [@implicit_arity] N_rejected(id, ())),
      ...acc,
    ]
  | N_accepted(_) => acc
  };

let get_node_ids_fold_fun = {
  ...Cnl2cnl.id_fold_over_node_fun,
  Cnl2cnl.poly_fold_fun: get_node_ids,
};

let rule_get_node_ids = (rule: cnl_rule): list((id, cnl_kind, node(unit))) =>
  Cnl2cnl.rule_dp_fold_over_nodes(get_node_ids_fold_fun, rule, []);

/** {8. Undefined} */;

let get_undefined =
    (kind: cnl_kind, node: node('a), acc: list((id, cnl_kind)))
    : list((id, cnl_kind)) =>
  switch (node) {
  | N_undefined(id) => [(id, kind), ...acc]
  | [@implicit_arity] N_filled(_, _)
  | [@implicit_arity] N_rejected(_, _)
  | N_accepted(_) => acc
  };

let get_undefined_fold_fun = {
  ...Cnl2cnl.id_fold_over_node_fun,
  Cnl2cnl.poly_fold_fun: get_undefined,
};

let rule_get_undefined = (rule: cnl_rule): list((id, cnl_kind)) =>
  Cnl2cnl.rule_dp_fold_over_nodes(get_undefined_fold_fun, rule, []);

let evnt_get_undefined = (evnt: cnl_event): list((id, cnl_kind)) =>
  Cnl2cnl.evnt_dp_fold_over_nodes(get_undefined_fold_fun, evnt, []);

let cond_get_undefined = (cond: cnl_cond): list((id, cnl_kind)) =>
  Cnl2cnl.cond_dp_fold_over_nodes(get_undefined_fold_fun, cond, []);

let actns_get_undefined = (actns: cnl_actions): list((id, cnl_kind)) =>
  Cnl2cnl.actns_dp_fold_over_nodes(get_undefined_fold_fun, actns, []);

let actn_get_undefined = (actn: cnl_action): list((id, cnl_kind)) =>
  Cnl2cnl.actn_dp_fold_over_nodes(get_undefined_fold_fun, actn, []);

let expr_get_undefined = (expr: cnl_expr): list((id, cnl_kind)) =>
  Cnl2cnl.expr_dp_fold_over_nodes(get_undefined_fold_fun, expr, []);

/** {8. Filled} */;

let get_filled =
    (kind: cnl_kind, node: node('a), acc: list((id, cnl_kind)))
    : list((id, cnl_kind)) =>
  switch (node) {
  | [@implicit_arity] N_filled(id, _) => [(id, kind), ...acc]
  | N_undefined(_)
  | [@implicit_arity] N_rejected(_, _)
  | N_accepted(_) => acc
  };

let get_filled_fold_fun = {
  ...Cnl2cnl.id_fold_over_node_fun,
  Cnl2cnl.poly_fold_fun: get_filled,
};

let rule_get_filled = (rule: cnl_rule): list((id, cnl_kind)) =>
  Cnl2cnl.rule_dp_fold_over_nodes(get_filled_fold_fun, rule, []);

let evnt_get_filled = (evnt: cnl_event): list((id, cnl_kind)) =>
  Cnl2cnl.evnt_dp_fold_over_nodes(get_filled_fold_fun, evnt, []);

let cond_get_filled = (cond: cnl_cond): list((id, cnl_kind)) =>
  Cnl2cnl.cond_dp_fold_over_nodes(get_filled_fold_fun, cond, []);

let actns_get_filled = (actns: cnl_actions): list((id, cnl_kind)) =>
  Cnl2cnl.actns_dp_fold_over_nodes(get_filled_fold_fun, actns, []);

let actn_get_filled = (actn: cnl_action): list((id, cnl_kind)) =>
  Cnl2cnl.actn_dp_fold_over_nodes(get_filled_fold_fun, actn, []);

let expr_get_filled = (expr: cnl_expr): list((id, cnl_kind)) =>
  Cnl2cnl.expr_dp_fold_over_nodes(get_filled_fold_fun, expr, []);

/** {8. Rejected} */;

let get_rejected =
    (kind: cnl_kind, node: node('a), acc: list((id, cnl_kind)))
    : list((id, cnl_kind)) =>
  switch (node) {
  | [@implicit_arity] N_rejected(id, _) => [(id, kind), ...acc]
  | N_undefined(_)
  | [@implicit_arity] N_filled(_, _)
  | N_accepted(_) => acc
  };

let get_rejected_fold_fun = {
  ...Cnl2cnl.id_fold_over_node_fun,
  Cnl2cnl.poly_fold_fun: get_rejected,
};

let rule_get_rejected = (rule: cnl_rule): list((id, cnl_kind)) =>
  Cnl2cnl.rule_dp_fold_over_nodes(get_rejected_fold_fun, rule, []);

let evnt_get_rejected = (evnt: cnl_event): list((id, cnl_kind)) =>
  Cnl2cnl.evnt_dp_fold_over_nodes(get_rejected_fold_fun, evnt, []);

let cond_get_rejected = (cond: cnl_cond): list((id, cnl_kind)) =>
  Cnl2cnl.cond_dp_fold_over_nodes(get_rejected_fold_fun, cond, []);

let actns_get_rejected = (actns: cnl_actions): list((id, cnl_kind)) =>
  Cnl2cnl.actns_dp_fold_over_nodes(get_rejected_fold_fun, actns, []);

let actn_get_rejected = (actn: cnl_action): list((id, cnl_kind)) =>
  Cnl2cnl.actn_dp_fold_over_nodes(get_rejected_fold_fun, actn, []);

let expr_get_rejected = (expr: cnl_expr): list((id, cnl_kind)) =>
  Cnl2cnl.expr_dp_fold_over_nodes(get_rejected_fold_fun, expr, []);

/** {6. Get subtree} */;

let rec expr_get_cnl = (id: id, expr: cnl_expr): option(cnl_ast) =>
  switch (node_id(expr.expr_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_expr(expr));
    } else {
      Cnl2cnl.expr_sh_fold(
        (expr, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => expr_get_cnl(id, expr)
          },
        expr,
        None,
      );
    }
  | None => None
  };

let evnt_get_cnl = (id: id, evnt: cnl_event): option(cnl_ast) =>
  switch (node_id(evnt.evnt_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_evnt(evnt));
    } else {
      Cnl2cnl.evnt_sh_fold(((), acc) => acc, evnt, None);
    }
  | None => None
  };

let cond_get_cnl = (id: id, cond: cnl_cond): option(cnl_ast) =>
  switch (node_id(cond.cond_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_cond(cond));
    } else {
      Cnl2cnl.cond_sh_fold(
        (expr, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => expr_get_cnl(id, expr)
          },
        cond,
        None,
      );
    }
  | None => None
  };

let actn_get_cnl = (id: id, actn: cnl_action): option(cnl_ast) =>
  switch (node_id(actn.actn_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_actn(actn));
    } else {
      Cnl2cnl.actn_sh_fold(
        (expr, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => expr_get_cnl(id, expr)
          },
        actn,
        None,
      );
    }
  | None => None
  };

let actns_get_cnl = (id: id, actns: cnl_actions): option(cnl_ast) =>
  switch (node_id(actns.actns_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_actns(actns));
    } else {
      Cnl2cnl.actns_sh_fold(
        (actn, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => actn_get_cnl(id, actn)
          },
        (closed, acc) => acc,
        actns,
        None,
      );
    }
  | None => None
  };

let rule_get_cnl = (id: id, rule: cnl_rule): option(cnl_ast) =>
  switch (node_id(rule.rule_node)) {
  | Some(n) =>
    if (id == n) {
      Some(Cnl_rule(rule));
    } else {
      Cnl2cnl.rule_sh_fold(
        (evnt, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => evnt_get_cnl(id, evnt)
          },
        (cond, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => cond_get_cnl(id, cond)
          },
        (actns, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None => actns_get_cnl(id, actns)
          },
        rule,
        None,
      );
    }
  | None => None
  };

/** {6. Find focus} */;

let rec expr_next_focus_fold =
        (
          focus: int,
          expr: cnl_expr,
          (res, id_found): (option((int, cnl_kind)), bool),
        )
        : (option((int, cnl_kind)), bool) =>
  switch (res, id_found, expr.expr_node) {
  | (Some(_), _, _) => (res, id_found)
  | (
      None,
      true,
      N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
    ) => (
      Some((id, K_expr(expr.expr_field))),
      true,
    )
  | (None, _, N_undefined(id)) => (None, false)
  | (
      None,
      _,
      [@implicit_arity] N_filled(Some(id), desc) |
      [@implicit_arity] N_rejected(Some(id), desc),
    ) =>
    if (id == focus) {
      let rejected = List.rev(expr_get_rejected(expr));
      let undefined = List.rev(expr_get_undefined(expr));
      switch (rejected, undefined) {
      | ([(Some(id), kind), ..._], _) => (Some((id, kind)), true)
      | ([], [(Some(id), kind), ..._]) => (Some((id, kind)), true)
      | ([], []) => (None, true)
      | ([(None, _), ..._], _)
      | ([], [(None, _), ..._]) => assert(false)
      };
    } else {
      Cnl2cnl.expr_sh_fold(
        expr_next_focus_fold(focus),
        expr,
        (res, id_found),
      );
    }
  | (None, _, _) =>
    Cnl2cnl.expr_sh_fold(expr_next_focus_fold(focus), expr, (res, id_found))
  };

let evnt_next_focus_fold =
    (
      focus: int,
      evnt: cnl_event,
      (res, id_found): (option((int, cnl_kind)), bool),
    )
    : (option((int, cnl_kind)), bool) =>
  switch (res, id_found, evnt.evnt_node) {
  | (Some(_), _, _) => (res, id_found)
  | (
      None,
      true,
      N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
    ) => (
      Some((id, K_evnt)),
      true,
    )
  | (_, _, _) =>
    let id_found = id_found || node_id(evnt.evnt_node) == Some(Some(focus));
    Cnl2cnl.evnt_sh_fold(((), acc) => acc, evnt, (None, id_found));
  };

let cond_next_focus_fold =
    (
      focus: int,
      cond: cnl_cond,
      (res, id_found): (option((int, cnl_kind)), bool),
    )
    : (option((int, cnl_kind)), bool) =>
  switch (res, id_found, cond.cond_node) {
  | (Some(_), _, _) => (res, id_found)
  | (
      None,
      true,
      N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
    ) => (
      Some((id, K_cond)),
      true,
    )
  | _ =>
    let id_found = id_found || node_id(cond.cond_node) == Some(Some(focus));
    Cnl2cnl.cond_sh_fold(
      expr_next_focus_fold(focus),
      cond,
      (None, id_found),
    );
  };

let actn_next_focus_fold =
    (
      focus: int,
      actn: cnl_action,
      (res, id_found): (option((int, cnl_kind)), bool),
    )
    : (option((int, cnl_kind)), bool) =>
  switch (res, id_found, actn.actn_node) {
  | (Some(_), _, _) => (res, id_found)
  | (
      None,
      true,
      N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
    ) => (
      Some((id, K_actn)),
      true,
    )
  | _ =>
    let id_found = id_found || node_id(actn.actn_node) == Some(Some(focus));
    Cnl2cnl.actn_sh_fold(
      expr_next_focus_fold(focus),
      actn,
      (None, id_found),
    );
  };

let actns_next_focus_fold =
    (
      focus: int,
      actns: cnl_actions,
      (res, id_found): (option((int, cnl_kind)), bool),
    )
    : (option((int, cnl_kind)), bool) =>
  switch (res, id_found, actns.actns_node) {
  | (Some(_), _, _) => (res, id_found)
  | (
      None,
      true,
      N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
    ) => (
      Some((id, K_actns)),
      true,
    )
  | _ =>
    let id_found = node_id(actns.actns_node) == Some(Some(focus));
    Cnl2cnl.actns_sh_fold(
      actn_next_focus_fold(focus),
      (closed, (res, id_found)) =>
        switch (res, id_found, closed) {
        | (Some(_), _, _) => (res, id_found)
        | (
            None,
            true,
            N_undefined(Some(id)) | [@implicit_arity] N_rejected(Some(id), _),
          ) => (
            Some((id, K_actns_closed)),
            true,
          )
        | _ =>
          let id_found = id_found || node_id(closed) == Some(Some(focus));
          (res, id_found);
        },
      actns,
      (None, id_found),
    );
  };

let rule_next_focus = (focus: int, rule: cnl_rule): option((int, cnl_kind)) => {
  let (res_opt, _) = {
    let id_found = node_id(rule.rule_node) == Some(Some(focus));
    Cnl2cnl.rule_sh_fold(
      evnt_next_focus_fold(focus),
      cond_next_focus_fold(focus),
      actns_next_focus_fold(focus),
      rule,
      (None, id_found),
    );
  };

  switch (res_opt) {
  | Some((id, kind)) => Some((id, kind))
  | None =>
    let rejected = List.rev(rule_get_rejected(rule));
    let undefined = List.rev(rule_get_undefined(rule));
    switch (rejected, undefined) {
    | ([(Some(id), kind), ..._], _) => Some((id, kind))
    | ([], [(Some(id), kind), ..._]) => Some((id, kind))
    | ([], []) => None
    | ([(None, _), ..._], _)
    | ([], [(None, _), ..._]) => assert(false)
    };
  };
};

let cond_next_focus = (focus: int, rule: cnl_rule): option((int, cnl_kind)) => {
  let (res_opt, _) = {
    let id_found = node_id(rule.rule_node) == Some(Some(focus));
    Cnl2cnl.rule_sh_fold(
      (_, acc) => acc,
      cond_next_focus_fold(focus),
      (_, acc) => acc,
      rule,
      (None, id_found),
    );
  };

  switch (res_opt, node_desc(rule.rule_node)) {
  | (Some((id, kind)), _) => Some((id, kind))
  | (None, Some(rule_desc)) =>
    let rejected = List.rev(cond_get_rejected(rule_desc.rule_cond));
    let undefined = List.rev(cond_get_undefined(rule_desc.rule_cond));
    switch (rejected, undefined) {
    | ([(Some(id), kind), ..._], _) => Some((id, kind))
    | ([], [(Some(id), kind), ..._]) => Some((id, kind))
    | ([], []) => None
    | ([(None, _), ..._], _)
    | ([], [(None, _), ..._]) => assert(false)
    };
  | (None, None) => None
  };
};

/** {6. Renaming } */;

let max_id = (rule: cnl_rule): id => {
  let fold_fun = (kind, node, m_id) => {
    let id =
      switch (node) {
      | N_undefined(id)
      | [@implicit_arity] N_filled(id, _)
      | [@implicit_arity] N_rejected(id, _) => id
      | N_accepted(_) => None
      };

    switch (id, m_id) {
    | (Some(id1), Some(id2)) => Some(max(id1, id2))
    | (Some(id), None)
    | (None, Some(id)) => Some(id)
    | (None, None) => None
    };
  };

  Cnl2cnl.rule_dp_fold_over_nodes(
    {...Cnl2cnl.id_fold_over_node_fun, Cnl2cnl.poly_fold_fun: fold_fun},
    rule,
    None,
  );
};

let next_id = (id: id): id =>
  switch (id) {
  | None => Some(1)
  | Some(n) => Some(n + 1)
  };

let index_rule = (rule: cnl_rule): cnl_rule => {
  let map_fold_fun = (kind, node, id) =>
    switch (node) {
    | N_undefined(None) => (N_undefined(id), next_id(id))
    | [@implicit_arity] N_filled(None, x) => (
        [@implicit_arity] N_filled(id, x),
        next_id(id),
      )
    | [@implicit_arity] N_rejected(None, x) => (
        [@implicit_arity] N_rejected(id, x),
        next_id(id),
      )
    | N_undefined(Some(_))
    | [@implicit_arity] N_filled(Some(_), _)
    | [@implicit_arity] N_rejected(Some(_), _)
    | N_accepted(_) => (node, id)
    };

  let (rule, _) =
    Cnl2cnl.rule_dp_map_fold_over_nodes(
      {
        ...Cnl2cnl.id_map_fold_over_node_fun,
        Cnl2cnl.poly_map_fold_fun: map_fold_fun,
      },
      rule,
      next_id(max_id(rule)),
    );

  rule;
};

/** {6. Change Node State} */;

/** {8. Filled to Accepted} */;

let f_to_a = p =>
  switch (p) {
  | N_undefined(_) => raise(Failure("Cannot confirm undefined AST node"))
  | [@implicit_arity] N_filled(_, ed) => N_accepted(ed)
  | N_rejected(_) => raise(Failure("Cannot confirm rejected AST node"))
  | N_accepted(ed) => N_accepted(ed)
  };

let expr_f_to_a = (expr: cnl_expr): cnl_expr =>
  Cnl2cnl.expr_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    expr,
  );

let actn_f_to_a = (actn: cnl_action): cnl_action =>
  Cnl2cnl.actn_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    actn,
  );

let evnt_f_to_a = (evnt: cnl_event): cnl_event =>
  Cnl2cnl.evnt_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    evnt,
  );

let cond_f_to_a = (cond: cnl_cond): cnl_cond =>
  Cnl2cnl.cond_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    cond,
  );

let actns_f_to_a = (actns: cnl_actions): cnl_actions =>
  Cnl2cnl.actns_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    actns,
  );

let rule_f_to_a = (rule: cnl_rule): cnl_rule =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_a(node),
    },
    rule,
  );

let cnl_f_to_a = (cnl: cnl_ast): cnl_ast =>
  switch (cnl) {
  | Cnl_expr(expr) => Cnl_expr(expr_f_to_a(expr))
  | Cnl_actn(actn) => Cnl_actn(actn_f_to_a(actn))
  | Cnl_evnt(evnt) => Cnl_evnt(evnt_f_to_a(evnt))
  | Cnl_cond(cond) => Cnl_cond(cond_f_to_a(cond))
  | Cnl_actns(actns) => Cnl_actns(actns_f_to_a(actns))
  | Cnl_rule(rule) => Cnl_rule(rule_f_to_a(rule))
  };

/** {8. Filled to Reject} */;

let f_to_r = p =>
  switch (p) {
  | N_undefined(_) => raise(Failure("Cannot confirm undefined AST node"))
  | [@implicit_arity] N_filled(id, ed) =>
    [@implicit_arity] N_rejected(id, ed)
  | N_rejected(_) => p
  | N_accepted(ed) => N_accepted(ed)
  };

let expr_f_to_r = (expr: cnl_expr): cnl_expr =>
  Cnl2cnl.expr_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    expr,
  );

let actn_f_to_r = (actn: cnl_action): cnl_action =>
  Cnl2cnl.actn_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    actn,
  );

let evnt_f_to_r = (evnt: cnl_event): cnl_event =>
  Cnl2cnl.evnt_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    evnt,
  );

let cond_f_to_r = (cond: cnl_cond): cnl_cond =>
  Cnl2cnl.cond_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    cond,
  );

let actns_f_to_r = (actns: cnl_actions): cnl_actions =>
  Cnl2cnl.actns_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    actns,
  );

let rule_f_to_r = (rule: cnl_rule): cnl_rule =>
  Cnl2cnl.rule_dp_map_over_nodes(
    {
      ...Cnl2cnl.id_map_over_node_fun,
      Cnl2cnl.poly_map_fun: (kind, node) => f_to_r(node),
    },
    rule,
  );

let cnl_f_to_r = (cnl: cnl_ast): cnl_ast =>
  switch (cnl) {
  | Cnl_expr(expr) => Cnl_expr(expr_f_to_r(expr))
  | Cnl_actn(actn) => Cnl_actn(actn_f_to_r(actn))
  | Cnl_evnt(evnt) => Cnl_evnt(evnt_f_to_r(evnt))
  | Cnl_cond(cond) => Cnl_cond(cond_f_to_r(cond))
  | Cnl_actns(actns) => Cnl_actns(actns_f_to_r(actns))
  | Cnl_rule(rule) => Cnl_rule(rule_f_to_r(rule))
  };

/** {6. modify tree } */;

let add_cond = rule => {
  let (id, rule_desc) =
    switch (rule.rule_node) {
    | N_undefined(id) => assert(false)
    | [@implicit_arity] N_rejected(id, _) => assert(false)
    | N_accepted(_) => assert(false)
    | [@implicit_arity] N_filled(id, rule_desc) => (id, rule_desc)
    };

  let cond =
    switch (rule_desc.rule_cond.cond_node) {
    | N_undefined(id) =>
      Cnl_builder.mk_cond_f(C_condition(Cnl_builder.mk_expr_undefined()))
    | [@implicit_arity] N_filled(_, C_condition(e))
    | N_accepted(C_condition(e)) =>
      let expr =
        Cnl_builder.mk_binop_expr_f(
          Op_and,
          e,
          Cnl_builder.mk_expr_undefined(),
        );

      Cnl_builder.mk_cond_f(C_condition(expr));
    | [@implicit_arity] N_filled(_, C_no_condition)
    | N_accepted(C_no_condition) =>
      Cnl_builder.mk_cond_f(C_condition(Cnl_builder.mk_expr_undefined()))
    | N_rejected(_) => assert(false)
    };

  let rule = {
    ...rule,
    rule_node:
      [@implicit_arity] N_filled(id, {...rule_desc, rule_cond: cond}),
  };

  index_rule(rule);
};

/** {6. prompt message} */;

exception Prompt(string);

let expr_prompt = (id, expr) => {
  let rec expr_prompt_aux = (id, txt, expr) =>
    switch (node_id(expr.expr_node), node_desc(expr.expr_node)) {
    | (Some(id'), _) when id == id' => raise(Prompt(txt))
    | (_, None) => ()
    | (_, Some(expr_desc)) =>
      switch (expr_desc) {
      | E_lit(_) => ()
      | E_var(_) => ()
      | [@implicit_arity] E_get(e, field) =>
        let txt =
          "the expression from which we get the field '" ++ field ++ "'";
        expr_prompt_aux(id, txt, e);
      | [@implicit_arity] E_agg(agg, e, field) =>
        let txt =
          "the operand of the '" ++ string_of_cnl_agg(agg) ++ "' aggregation";

        expr_prompt_aux(id, txt, e);
      | [@implicit_arity] E_unop(op, e) =>
        let txt =
          "the operand of the '" ++ string_of_cnl_unop(op) ++ "' operator";

        expr_prompt_aux(id, txt, e);
      | [@implicit_arity] E_binop(op, e1, e2) =>
        let txt1 =
          "the left operand of the '"
          ++ string_of_cnl_binop(op)
          ++ "' operator";

        let () = expr_prompt_aux(id, txt1, e1);
        let txt2 =
          "the right operand of the '"
          ++ string_of_cnl_binop(op)
          ++ "' operator";

        let () = expr_prompt_aux(id, txt2, e2);
        ();
      | E_error(_) => ()
      | E_this(_) => ()
      | [@implicit_arity] E_new(evnt, setters) =>
        List.iter(
          ((field, e)) => {
            let txt = "the expression defining the field '" ++ field ++ "'";
            let _ = expr_prompt_aux(id, txt, e);
            ();
          },
          setters,
        );
        ();
      }
    };

  try(
    {
      expr_prompt_aux(id, "", expr);
      "";
    }
  ) {
  | Prompt(txt) => txt
  };
};

let rule_prompt = (id, rule) =>
  Cnl2cnl.rule_sh_fold(
    (_, acc) => acc,
    (cond, acc) =>
      Cnl2cnl.cond_sh_fold(
        (expr, acc) =>
          switch (acc) {
          | Some(_) => acc
          | None =>
            let s = expr_prompt(id, expr);
            if (s == "") {
              None;
            } else {
              Some(s);
            };
          },
        cond,
        acc,
      ),
    (actns, acc) =>
      Cnl2cnl.actns_sh_fold(
        (actn, acc) =>
          Cnl2cnl.actn_sh_fold(
            (expr, acc) =>
              switch (acc) {
              | Some(_) => acc
              | None =>
                let s = expr_prompt(id, expr);
                if (s == "") {
                  None;
                } else {
                  Some(s);
                };
              },
            actn,
            acc,
          ),
        (_, acc) => acc,
        actns,
        acc,
      ),
    rule,
    None,
  );

/** {6. Conversion to Yojson.Basic} */;

let json_of_expr = expr => Yojson.Safe.to_basic(cnl_expr_to_yojson(expr));
let json_of_expr_desc = desc =>
  Yojson.Safe.to_basic(cnl_expr_desc_to_yojson(desc));

let json_of_evnt = evnt => Yojson.Safe.to_basic(cnl_event_to_yojson(evnt));
let json_of_evnt_desc = desc =>
  Yojson.Safe.to_basic(cnl_evnt_desc_to_yojson(desc));

let json_of_cond = cond => Yojson.Safe.to_basic(cnl_cond_to_yojson(cond));
let json_of_cond_desc = desc =>
  Yojson.Safe.to_basic(cnl_cond_desc_to_yojson(desc));

let json_of_actn = actn => Yojson.Safe.to_basic(cnl_action_to_yojson(actn));
let json_of_actn_desc = desc =>
  Yojson.Safe.to_basic(cnl_actn_desc_to_yojson(desc));

let json_of_actns = actns =>
  Yojson.Safe.to_basic(cnl_actions_to_yojson(actns));
let json_of_actns_desc = desc =>
  Yojson.Safe.to_basic(cnl_actns_desc_to_yojson(desc));

let json_of_rule = rule => Yojson.Safe.to_basic(cnl_rule_to_yojson(rule));
let json_of_rule_desc = desc =>
  Yojson.Safe.to_basic(cnl_rule_desc_to_yojson(desc));

let rec json_replace = (old: string, new_: string, j: Yojson.Basic.t) =>
  switch (j) {
  | `Null
  | `Bool(_)
  | `Int(_)
  | `Float(_) => j
  | `String(s) =>
    if (s == old) {
      `String(new_);
    } else {
      j;
    }
  | `Assoc(l) =>
    let l =
      List.map(
        ((s, j)) => {
          let s =
            if (s == old) {
              new_;
            } else {
              s;
            };
          let j = json_replace(old, new_, j);
          (s, j);
        },
        l,
      );

    `Assoc(l);
  | `List(l) =>
    let l = List.map(j => json_replace(old, new_, j), l);
    `List(l);
  };
