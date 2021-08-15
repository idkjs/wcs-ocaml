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
open Dialog_t;

let mk_conditions = cond =>
  switch (cond) {
  | None => None
  | Some(cond) => Some(Spel.of_string(cond))
  };

let mk_context = context =>
  switch (context) {
  | None => None
  | Some(context) => Some(Json_spel.of_json(context))
  };

let mk_output = output =>
  switch (output) {
  | None => None
  | Some(output) => Some(Json_spel.of_json(output))
  };

let dialog_node_of_node = (n: node): list(dialog_node) => {
  let root = {
    node_dialog_node: n.n_dialog_node,
    node_type_: None,
    node_description: n.n_description,
    node_conditions: mk_conditions(n.n_conditions),
    node_parent: None,
    node_previous_sibling: None,
    node_output: None,
    node_context: None,
    node_metadata: n.n_metadata,
    node_next_step: n.n_next_step,
    node_child_input_kind: None,
    node_created: n.n_created,
    node_updated: n.n_updated,
    node_event_name: None,
    node_variable: None,
  };

  let children =
    switch (n.n_reactions) {
    | [] => []
    | _ => assert(false) /* XXX TODO XXX */
    };

  let children =
    switch (n.n_slots) {
    | [] => children
    | _ => assert(false) /* XXX TODO XXX */
    };

  let (root, children) =
    switch (n.n_responses) {
    | [] => (root, children)
    | [{r_conditions: None, r_output: output, r_context: context}] =>
      let root = {
        ...root,
        node_output: mk_output(output),
        node_context: mk_context(context),
      };

      (root, children);
    | _ => assert(false) /* XXX TODO XXX */
    };

  [root, ...children];
};

let dialog_nodes_of_dialog = (d: dialog): list(dialog_node) => {
  let rec compile = d =>
    switch (d) {
    | D([]) => []
    | D(l) =>
      List.fold_right(
        ((n, d), acc) => {
          let children = compile(d);
          let dn_tree = dialog_node_of_node(n);
          let root = Wcs.get_root(dn_tree);
          let dn_tree = Wcs.add_tree(dn_tree, children, root, None);

          Wcs.add_tree(dn_tree, acc, None, root);
        },
        l,
        [],
      )
    };

  compile(d);
};
