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

type mode =
  | M_nothing
  | M_bot
  | M_ws_gen
  | M_ws_delete;

let rulebot_mode: ref(mode);
let wcs_credential: ref(option(string));
let workspaces_config: ref(option(Dialog_interface_t.config));
let ws_update: ref(bool);
let is_slack: ref(bool);
let slackbot: ref(string);
let bom_io: ref(option(string));
let load_io: string => Io_t.io;

let load_ws_ids:
  (
    Wcs_t.credential,
    option(Dialog_interface_t.config),
    bool,
    (string, Bmd_t.bmd_schema)
  ) =>
  Dialog_util.workspace_ids;

let bmd: ref(option(string));
let args: list((Arg.key, Arg.spec, Arg.doc));
let anon_args: string => unit;
let usage: string;

let workspaces_generation: ((string, Bmd_t.bmd_schema)) => unit;
let workspaces_delete: Wcs_t.credential => unit;
