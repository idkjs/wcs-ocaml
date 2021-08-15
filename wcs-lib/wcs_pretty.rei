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

/** Pretty print Wcs data structures as JSON objects. */;

open Wcs_t;

/** workspace_response */

let workspace_response: workspace_response => string;

/** pagination_response */

let pagination_response: pagination_response => string;

/** list_workspaces_request */

let list_workspaces_request: list_workspaces_request => string;

/** list_workspaces_response */

let list_workspaces_response: list_workspaces_response => string;

/** intent_example */

let intent_example: intent_example => string;

/** intent_def */

let intent_def: intent_def => string;

/** entity_value */

let entity_value: entity_value => string;

/** entity_def */

let entity_def: entity_def => string;

/** next_step */

let next_step: next_step => string;

/** output_def */

let output_def: output_def => string;

/** dialog_node */

let dialog_node: dialog_node => string;

/** workspace */

let workspace: workspace => string;

/** input */

let input: input => string;

/** entity */

let entity: entity => string;

/** output */

let output: output => string;

/** message_request */

let message_request: message_request => string;

/** message_response */

let message_response: message_response => string;

/** create_response */

let create_response: create_response => string;

/** get_workspace_request */

let get_workspace_request: get_workspace_request => string;

/** action */

let action: action => string;

/** action_def */

let action_def: action_def => string;

/** log_entry */

let log_entry: log_entry => string;

/** logs_request */

let logs_request: logs_request => string;

/** logs_response */

let logs_response: logs_response => string;
