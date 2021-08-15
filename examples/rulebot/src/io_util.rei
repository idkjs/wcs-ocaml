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

let slack_io: ref(bool);
let set_slack_io: unit => unit;

let set_slack_log: string => unit;
let close_slack_log: unit => unit;

let pretty_json_string: string => string;

let print_rule: Cnl_t.cnl_rule => unit;

let print_workspace: string => unit;

let print_instr: int => unit;
let print_berl_error: string => unit;

let print_done: unit => unit;

let print_C: string => unit;
let print_output_stdout: (option(Cnl_t.cnl_rule), string) => unit;
let get_input_stdin: unit => string;
