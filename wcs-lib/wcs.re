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

let omap = (f, o) =>
  switch (o) {
  | None => None
  | Some(x) => Some(f(x))
  };

let new_id = {
  let cpt = ref(0);
  () => {
    incr(cpt);
    "node_" ++ string_of_int(cpt^);
  };
};

let list_workspaces_request =
    /* ?version */
    (~page_limit=?, ~include_count=?, ~sort=?, ~cursor=?, ())
    : list_workspaces_request => {
  /* list_ws_req_version = version; */
  list_ws_req_page_limit: page_limit,
  list_ws_req_include_count: include_count,
  list_ws_req_sort: sort,
  list_ws_req_cursor: cursor,
};

let get_workspace_request = (~export=?, workspace_id) => {
  get_ws_req_workspace_id: workspace_id,
  get_ws_req_export: export,
};

let example = (example, ~created=?, ~updated=?, ()): intent_example => {
  ex_text: example,
  ex_created: created,
  ex_updated: updated,
};

let intent =
    (intent, ~description=?, ~examples=[], ~created=?, ~updated=?, ())
    : intent_def => {
  i_def_intent: intent,
  i_def_description: description,
  i_def_examples: List.map(s => example(s, ()), examples),
  i_def_created: created,
  i_def_updated: updated,
};

let value =
    (value, ~metadata=?, ~synonyms=[], ~created=?, ~updated=?, ())
    : entity_value => {
  e_val_value: value,
  e_val_metadata: metadata,
  e_val_synonyms: synonyms,
  e_val_created: created,
  e_val_updated: updated,
};

let entity =
    (
      entity,
      ~description=?,
      ~metadata=?,
      ~source=?,
      ~open_list=?,
      ~values=[],
      ~created=?,
      ~updated=?,
      ~fuzzy_match=?,
      (),
    )
    : entity_def => {
  e_def_entity: entity,
  e_def_description: description,
  e_def_metadata: metadata,
  e_def_source: source,
  e_def_open_list: open_list,
  e_def_values:
    List.map(((v, syn)) => value(v, ~synonyms=syn, ()), values),
  e_def_created: created,
  e_def_updated: updated,
  e_def_fuzzy_match: fuzzy_match,
};

let next_step = (node, ~selector, ()): next_step => {
  next_behavior: "jump_to",
  next_selector: selector,
  next_dialog_node: node.node_dialog_node,
};

let mk_next_step = next_step; /* alias to avoid hiding */

let next_step_id = (node_id, ~selector, ()): next_step => {
  next_behavior: "jump_to",
  next_selector: selector,
  next_dialog_node: node_id,
};

let mk_next_step_id = next_step_id; /* alias to avoid hiding */

let output = /* XXX TODO : handle multiple outputs XXX */
    (text): output_def =>
  Json_spel.of_json(`Assoc([("text", `String(text))]));

let mk_output = output; /* alias to avoid hiding */

let dialog_node =
    (
      dialog_node,
      ~description=?,
      ~type_=?,
      ~conditions=?,
      ~conditions_spel=?,
      ~parent=?,
      ~parent_id=?,
      ~previous_sibling=?,
      ~previous_sibling_id=?,
      ~text=?,
      ~text_spel=?,
      ~output=?,
      ~output_spel=?,
      ~context=?,
      ~context_spel=?,
      ~metadata=?,
      ~next_step=?,
      ~next_step_id=?,
      ~created=?,
      ~updated=?,
      ~event_name=?,
      ~variable=?,
      (),
    )
    : dialog_node => {
  let parent_id =
    switch (parent, parent_id) {
    | (None, None) => None
    | (Some(p), None) => Some(p.node_dialog_node)
    | (None, Some(id)) => Some(id)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~parent and ~parent_id cannot be present simultanously",
      )
    };

  let previous_sibling_id =
    switch (previous_sibling, previous_sibling_id) {
    | (None, None) => None
    | (Some(s), None) => Some(s.node_dialog_node)
    | (None, Some(id)) => Some(id)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~previous_sibling and ~previous_sibling_id cannot be present simultanously",
      )
    };

  let text =
    switch (text, text_spel) {
    | (None, None) => None
    | (Some(text), None) => Some(mk_output(text))
    | (None, Some(expr)) => Some(mk_output(Spel_print.to_text(expr)))
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~text and ~text_spel cannot be present simultanously",
      )
    };

  let output =
    switch (output, output_spel) {
    | (None, None) => None
    | (Some(output), None) => Some(Json_spel.of_json(output))
    | (None, Some(json_expr)) => Some(json_expr)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~output and ~output_spel cannot be present simultanously",
      )
    };

  let output =
    switch (text, output) {
    | (None, None) => None
    | (Some(text), None) => Some(text)
    | (None, Some(output)) => Some(output)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~text and ~output cannot be present simultanously",
      )
    };

  let context =
    switch (context, context_spel) {
    | (None, None) => None
    | (Some(context), None) => Some(Json_spel.of_json(context))
    | (None, Some(json_expr)) => Some(json_expr)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~context and ~context_spel cannot be present simultanously",
      )
    };

  let next_step =
    switch (next_step, next_step_id) {
    | (None, None) => None
    | (Some((node, selector)), None) =>
      Some(mk_next_step(node, ~selector, ()))
    | (None, Some((node_id, selector))) =>
      Some(mk_next_step_id(node_id, ~selector, ()))
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~next_step and ~next_step_id cannot be present simultanously",
      )
    };

  let conditions =
    switch (conditions, conditions_spel) {
    | (None, None) => Some(Spel.bool(true))
    | (Some(text), None) => Some(Spel.of_string(text))
    | (None, Some(expr)) => Some(expr)
    | (Some(_), Some(_)) =>
      Log.error(
        "Wcs",
        Some(None),
        "dialog_node: ~conditions and ~conditions_spel cannot be present simultanously",
      )
    };

  {
    node_dialog_node: dialog_node,
    node_description: description,
    node_type_: type_,
    node_conditions: conditions,
    node_parent: parent_id,
    node_previous_sibling: previous_sibling_id,
    node_output: output,
    node_context: context,
    node_metadata: metadata,
    node_next_step: next_step,
    node_created: created,
    node_updated: updated,
    node_child_input_kind: None,
    node_event_name: event_name,
    node_variable: variable,
  };
};

let response_condition = (~parent) =>
  dialog_node(
    new_id(),
    ~type_=Node_response_condition,
    ~parent,
    ~parent_id=?None,
    ~next_step=?None, /* Not yet implemented*/
    ~next_step_id=?None, /* Not yet implemented*/
    ~event_name=?None,
    ~variable=?None,
  );

let fix_links = nodes => {
  let parent_child_tbl = Hashtbl.create(7);
  let node_tbl = Hashtbl.create(7);
  List.map(
    node => {
      let node =
        switch (node.node_parent, node.node_previous_sibling) {
        | (Some(_), Some(_)) => node
        | (Some(_), None)
        | (None, None) =>
          try({
            let previous_sibling =
              Hashtbl.find(parent_child_tbl, node.node_parent);

            {
              ...node,
              node_previous_sibling: Some(previous_sibling.node_dialog_node),
            };
          }) {
          | Not_found => node
          }
        | (None, Some(previous_sibling_name)) =>
          try({
            let previous_sibling =
              Hashtbl.find(node_tbl, previous_sibling_name);

            {...node, node_parent: previous_sibling.node_parent};
          }) {
          | Not_found => node
          }
        };

      Hashtbl.add(parent_child_tbl, node.node_parent, node);
      Hashtbl.add(node_tbl, node.node_dialog_node, node);
      node;
    },
    nodes,
  );
};

let workspace =
    (
      name,
      ~description=?,
      ~language=?,
      ~metadata=?,
      ~counterexamples=[],
      ~dialog_nodes=[],
      ~entities=[],
      ~intents=[],
      ~created=?,
      ~updated=?,
      ~modified=?,
      ~created_by=?,
      ~modified_by=?,
      ~workspace_id=?,
      ~status=?,
      (),
    )
    : workspace => {
  let counterexamples = List.map(s => example(s, ()), counterexamples);

  {
    ws_name: Some(name),
    ws_description: description,
    ws_language: language,
    ws_metadata: metadata,
    ws_counterexamples: counterexamples,
    ws_dialog_nodes: fix_links(dialog_nodes),
    ws_entities: entities,
    ws_intents: intents,
    ws_created: created,
    ws_updated: updated,
    ws_modified: modified,
    ws_created_by: created_by,
    ws_modified_by: modified_by,
    ws_workspace_id: workspace_id,
    ws_status: status,
  };
};

let logs_request =
    /* ?version */
    (~filter=?, ~sort=?, ~page_limit=?, ~cursor=?, ()): logs_request => {
  logs_filter: filter,
  logs_sort: sort,
  logs_page_limit: page_limit,
  logs_cursor: cursor,
};

let action_def =
    (
      name,
      ~agent="client",
      ~type_="conversation",
      ~parameters=Json_spel.null,
      ~result_variable=?,
      (),
    )
    : action_def => {
  act_def_name: name,
  act_def_agent: agent,
  act_def_type_: type_,
  act_def_parameters: parameters,
  act_def_result_variable: result_variable,
};

let action =
    (
      name,
      ~agent="client",
      ~type_="conversation",
      ~parameters=Json.null,
      ~result_variable=?,
      (),
    )
    : action => {
  act_name: name,
  act_agent: agent,
  act_type_: type_,
  act_parameters: parameters,
  act_result_variable: result_variable,
};

let sys_number: entity_def = (
  entity("sys-number", ~source="system.entities", ()): entity_def
);

/** {6. Message} */;

let message_request =
    (
      ~text=?,
      ~input=?,
      ~alternate_intents=?,
      ~context=?,
      ~entities=?,
      ~intents=?,
      ~output=?,
      (),
    )
    : message_request => {
  let input =
    switch (text, input) {
    | (Some(text), None) => {in_text: text}
    | (None, Some(input)) => input
    | (Some(text), Some(input)) =>
      Log.error(
        "Wcs",
        Some(input),
        "message_request: ~text and ~input cannot be present simultanously",
      )
    | (None, None) =>
      Log.error(
        "Wcs",
        Some({in_text: ""}),
        "message_request: ~text or ~input must be present",
      )
    };

  let alternate_intents =
    switch (alternate_intents) {
    | Some(b) => b
    | None => false
    };

  {
    msg_req_input: input,
    msg_req_alternate_intents: alternate_intents,
    msg_req_context: context,
    msg_req_entities: entities,
    msg_req_intents: intents,
    msg_req_output: output,
  };
};

/** {6 Tree manipulation} */;

let get_root = (tree): option(dialog_node) =>
  try(
    Some(
      List.find(
        x => x.node_parent === None && x.node_previous_sibling === None,
        tree,
      ),
    )
  ) {
  | Not_found => None
  };

let get_name = omap(x => x.node_dialog_node);

let add_node =
    (dialog_nodes, dialog_node, parent, previous_sibling): list(dialog_node) => {
  let node = {
    ...dialog_node,
    node_parent: get_name(parent),
    node_previous_sibling: get_name(previous_sibling),
  };

  let dialog_nodes =
    List.map(
      n =>
        if (n.node_previous_sibling == get_name(previous_sibling)) {
          {...n, node_previous_sibling: Some(node.node_dialog_node)};
        } else {
          n;
        },
      dialog_nodes,
    );

  [node, ...dialog_nodes];
};

let last_sibling = (dialog_node, dialog_nodes) => {
  let is_last_sibling = x => {
    let x_is_previous_sibling =
      List.exists(
        y => Some(x.node_dialog_node) == y.node_previous_sibling,
        dialog_nodes,
      );

    x.node_parent === dialog_node.node_parent && !x_is_previous_sibling;
  };

  try(Some(List.find(is_last_sibling, dialog_nodes))) {
  | Not_found => None
  };
};

let add_tree =
    (dialog_nodes, tree, parent, previous_sibling): list(dialog_node) => {
  let root =
    switch (get_root(tree)) {
    | Some(root) => root
    | None => Log.error("Wcs", None, "add_tree: tree has no root")
    };

  let last = last_sibling(root, tree);
  let dialog_nodes =
    List.map(
      n =>
        if (n.node_previous_sibling == get_name(previous_sibling)) {
          {...n, node_previous_sibling: get_name(last)};
        } else {
          n;
        },
      dialog_nodes,
    );

  let tree =
    List.map(
      n =>
        switch (n.node_parent, n.node_previous_sibling) {
        | (None, None) => {
            ...n,
            node_parent: get_name(parent),
            node_previous_sibling: get_name(previous_sibling),
          }
        | (None, Some(_)) => {...n, node_parent: get_name(parent)}
        | _ => n
        },
      tree,
    );

  dialog_nodes @ tree;
};

/** {6 Json conversion} */;

/** {8 Conversion of Wcs data structures to JSON} */;

let json_of_workspace_response = rsp =>
  Yojson.Basic.from_string(Wcs_j.string_of_workspace_response(rsp));

let json_of_pagination_response = rsp =>
  Yojson.Basic.from_string(Wcs_j.string_of_pagination_response(rsp));

let json_of_list_workspaces_request = req =>
  Yojson.Basic.from_string(Wcs_j.string_of_list_workspaces_request(req));

let json_of_list_workspaces_response = rsp =>
  Yojson.Basic.from_string(Wcs_j.string_of_list_workspaces_response(rsp));

let json_of_intent_example = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_intent_example(x));

let json_of_intent_def = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_intent_def(x));

let json_of_entity_value = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_entity_value(x));

let json_of_entity_def = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_entity_def(x));

let json_of_next_step = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_next_step(x));

let json_of_output_def = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_output_def(x));

let json_of_dialog_node = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_dialog_node(x));

let json_of_workspace = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_workspace(x));

let json_of_input = x => Yojson.Basic.from_string(Wcs_j.string_of_input(x));

let json_of_entity = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_entity(x));

let json_of_output = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_output(x));

let json_of_message_request = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_message_request(x));

let json_of_message_response = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_message_response(x));

let json_of_create_response = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_create_response(x));

let json_of_get_workspace_request = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_get_workspace_request(x));

let json_of_log_entry = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_log_entry(x));

let json_of_action = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_action(x));

let json_of_action_def = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_action_def(x));

let json_of_logs_request = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_logs_request(x));

let json_of_logs_response = x =>
  Yojson.Basic.from_string(Wcs_j.string_of_logs_response(x));

/** {8 Conversion of Wcs data structures to JSON with embedded Spel} */;

let json_spel_of_workspace_response = rsp =>
  Json_spel.of_json(json_of_workspace_response(rsp));

let json_spel_of_pagination_response = rsp =>
  Json_spel.of_json(json_of_pagination_response(rsp));

let json_spel_of_list_workspaces_request = req =>
  Json_spel.of_json(json_of_list_workspaces_request(req));

let json_spel_of_list_workspaces_response = rsp =>
  Json_spel.of_json(json_of_list_workspaces_response(rsp));

let json_spel_of_intent_example = x =>
  Json_spel.of_json(json_of_intent_example(x));

let json_spel_of_intent_def = x => Json_spel.of_json(json_of_intent_def(x));

let json_spel_of_entity_value = x =>
  Json_spel.of_json(json_of_entity_value(x));

let json_spel_of_entity_def = x => Json_spel.of_json(json_of_entity_def(x));

let json_spel_of_next_step = x => Json_spel.of_json(json_of_next_step(x));

let json_spel_of_output_def = x => Json_spel.of_json(json_of_output_def(x));

let json_spel_of_dialog_node = x =>
  Json_spel.of_json(json_of_dialog_node(x));

let json_spel_of_workspace = x => Json_spel.of_json(json_of_workspace(x));

let json_spel_of_input = x => Json_spel.of_json(json_of_input(x));

let json_spel_of_entity = x => Json_spel.of_json(json_of_entity(x));

let json_spel_of_output = x => Json_spel.of_json(json_of_output(x));

let json_spel_of_message_request = x =>
  Json_spel.of_json(json_of_message_request(x));

let json_spel_of_message_response = x =>
  Json_spel.of_json(json_of_message_response(x));

let json_spel_of_create_response = x =>
  Json_spel.of_json(json_of_create_response(x));

let json_spel_of_get_workspace_request = x =>
  Json_spel.of_json(json_of_get_workspace_request(x));

let json_spel_of_log_entry = x => Json_spel.of_json(json_of_log_entry(x));

let json_spel_of_action = x => Json_spel.of_json(json_of_action(x));

let json_spel_of_action_def = x => Json_spel.of_json(json_of_action_def(x));

let json_spel_of_logs_request = x =>
  Json_spel.of_json(json_of_logs_request(x));

let json_spel_of_logs_response = x =>
  Json_spel.of_json(json_of_logs_response(x));
