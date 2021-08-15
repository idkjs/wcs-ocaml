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

/** Type of a condtional response */

type response = {
  r_conditions: option(string),
  r_output: option(json),
  r_context: option(json),
};

/** Type of a slot */

type slot = {
  s_prompt: output_def,
  s_conditions: string,
  s_variable: string,
  s_context: option(json),
  s_match: list(response),
  s_nomatch: list(response),
};

/** Type of dialog nodes. */

type node = {
  n_dialog_node: string,
  n_description: option(string),
  n_conditions: option(string),
  n_prompt: output_def,
  n_reactions: list(response),
  n_responses: list(response),
  n_slots: list(slot),
  n_metadata: option(json),
  n_next_step: option(next_step),
  n_created: option(string),
  n_updated: option(string),
};

/** Type of dialog trees. */

type dialog =
  | D(list((node, dialog)));
