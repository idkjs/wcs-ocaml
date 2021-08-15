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

/** Wcs data structure constructors. */;

open Wcs_t;

/** {6 Builders} */;

let list_workspaces_request:
  (
    ~page_limit: int=?,
    ~include_count: bool=?,
    ~sort: sort_workspace_criteria=?,
    ~cursor: string=?,
    unit
  ) =>
  list_workspaces_request;

let get_workspace_request: (~export: bool=?, string) => get_workspace_request;

let example:
  (string, ~created: string=?, ~updated: string=?, unit) => intent_example;

let intent:
  (
    string,
    ~description: string=?,
    ~examples: list(string)=?,
    ~created: string=?,
    ~updated: string=?,
    unit
  ) =>
  intent_def;

let value:
  (
    string,
    ~metadata: json=?,
    ~synonyms: list(string)=?,
    ~created: string=?,
    ~updated: string=?,
    unit
  ) =>
  entity_value;

let entity:
  (
    string,
    ~description: string=?,
    ~metadata: json=?,
    ~source: string=?,
    ~open_list: bool=?,
    ~values: list((string, list(string)))=?,
    ~created: string=?,
    ~updated: string=?,
    ~fuzzy_match: bool=?,
    unit
  ) =>
  entity_def;

let next_step: (dialog_node, ~selector: selector, unit) => next_step;

let next_step_id: (string, ~selector: selector, unit) => next_step;

let output: string => output_def;

let dialog_node:
  (
    string,
    ~description: string=?,
    ~type_: dialog_node_type=?,
    ~conditions: string=?,
    ~conditions_spel: Spel_t.expression=?,
    ~parent: dialog_node=?,
    ~parent_id: string=?,
    ~previous_sibling: dialog_node=?,
    ~previous_sibling_id: string=?,
    ~text: string=?,
    ~text_spel: Spel_t.expression=?,
    ~output: json=?,
    ~output_spel: json_spel=?,
    ~context: json=?,
    ~context_spel: json_spel=?,
    ~metadata: json=?,
    ~next_step: (dialog_node, selector)=?,
    ~next_step_id: (string, selector)=?,
    ~created: string=?,
    ~updated: string=?,
    ~event_name: dialog_node_event_name=?,
    ~variable: string=?,
    unit
  ) =>
  dialog_node;

let response_condition:
  (
    ~parent: dialog_node,
    ~description: string=?,
    ~conditions: string=?,
    ~conditions_spel: Spel_t.expression=?,
    ~previous_sibling: dialog_node=?,
    ~previous_sibling_id: string=?,
    ~text: string=?,
    ~text_spel: Spel_t.expression=?,
    ~output: json=?,
    ~output_spel: json_spel=?,
    ~context: json=?,
    ~context_spel: json_spel=?,
    ~metadata: json=?,
    ~created: string=?,
    ~updated: string=?,
    unit
  ) =>
  dialog_node;

let workspace:
  (
    string,
    ~description: string=?,
    ~language: string=?,
    ~metadata: json=?,
    ~counterexamples: list(string)=?,
    ~dialog_nodes: list(dialog_node)=?,
    ~entities: list(entity_def)=?,
    ~intents: list(intent_def)=?,
    ~created: string=?,
    ~updated: string=?,
    ~modified: string=?,
    ~created_by: string=?,
    ~modified_by: string=?,
    ~workspace_id: string=?,
    ~status: workspace_status=?,
    unit
  ) =>
  workspace;

let logs_request:
  (
    ~filter: string=?,
    ~sort: sort_logs_criteria=?,
    ~page_limit: int=?,
    ~cursor: string=?,
    unit
  ) =>
  logs_request;

let sys_number: entity_def;

let action_def:
  (
    string,
    ~agent: string=?,
    ~type_: string=?,
    ~parameters: json_spel=?,
    ~result_variable: string=?,
    unit
  ) =>
  action_def;

let action:
  (
    string,
    ~agent: string=?,
    ~type_: string=?,
    ~parameters: json=?,
    ~result_variable: string=?,
    unit
  ) =>
  action;

/** {8 Message} */;

let message_request:
  (
    ~text: string=?,
    ~input: input=?,
    ~alternate_intents: bool=?,
    ~context: json=?,
    ~entities: list(entity)=?,
    ~intents: list(intent)=?,
    ~output: output=?,
    unit
  ) =>
  message_request;

/** {6 Tree manipulation} */;

/** [add_tree tree subtree parent previous_sibling] add the tree
    [subtree] in the dialog [tree]. The root of [subtree] is attached
    at the position defined with [parent] and [previous_sibling]. If
    there was already a node at this postion, it becomes the last
    sibling of the root of [subtree].
*/

let add_tree:
  (
    list(dialog_node),
    list(dialog_node),
    option(dialog_node),
    option(dialog_node)
  ) =>
  list(dialog_node);

/** [get_root tree] return the root of the dialog tree [tree]. It
    returns [None] if the tree is empty.
*/

let get_root: list(dialog_node) => option(dialog_node);

/** {6 Json conversion} */;

/** {8 Conversion of Wcs data structures to JSON} */;

let json_of_workspace_response: workspace_response => json;

let json_of_pagination_response: pagination_response => json;

let json_of_list_workspaces_request: list_workspaces_request => json;

let json_of_list_workspaces_response: list_workspaces_response => json;

let json_of_intent_example: intent_example => json;

let json_of_intent_def: intent_def => json;

let json_of_entity_value: entity_value => json;

let json_of_entity_def: entity_def => json;

let json_of_next_step: next_step => json;

let json_of_output_def: output_def => json;

let json_of_dialog_node: dialog_node => json;

let json_of_workspace: workspace => json;

let json_of_input: input => json;

let json_of_entity: entity => json;

let json_of_output: output => json;

let json_of_message_request: message_request => json;

let json_of_message_response: message_response => json;

let json_of_create_response: create_response => json;

let json_of_get_workspace_request: get_workspace_request => json;

let json_of_action: action => json;

let json_of_action_def: action_def => json;

let json_of_log_entry: log_entry => json;

let json_of_logs_request: logs_request => json;

let json_of_logs_response: logs_response => json;

/** {8 Conversion of Wcs data structures to JSON with embedded Spel} */;

let json_spel_of_workspace_response: workspace_response => json_spel;

let json_spel_of_pagination_response: pagination_response => json_spel;

let json_spel_of_list_workspaces_request: list_workspaces_request => json_spel;

let json_spel_of_list_workspaces_response:
  list_workspaces_response => json_spel;

let json_spel_of_intent_example: intent_example => json_spel;

let json_spel_of_intent_def: intent_def => json_spel;

let json_spel_of_entity_value: entity_value => json_spel;

let json_spel_of_entity_def: entity_def => json_spel;

let json_spel_of_next_step: next_step => json_spel;

let json_spel_of_output_def: output_def => json_spel;

let json_spel_of_dialog_node: dialog_node => json_spel;

let json_spel_of_workspace: workspace => json_spel;

let json_spel_of_input: input => json_spel;

let json_spel_of_entity: entity => json_spel;

let json_spel_of_output: output => json_spel;

let json_spel_of_message_request: message_request => json_spel;

let json_spel_of_message_response: message_response => json_spel;

let json_spel_of_create_response: create_response => json_spel;

let json_spel_of_get_workspace_request: get_workspace_request => json_spel;

let json_spel_of_action: action => json_spel;

let json_spel_of_action_def: action_def => json_spel;

let json_spel_of_log_entry: log_entry => json_spel;

let json_spel_of_logs_request: logs_request => json_spel;

let json_spel_of_logs_response: logs_response => json_spel;
