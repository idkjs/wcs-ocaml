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

exception Error(string, string);

let column = s =>
  if (s == "") {
    "";
  } else {
    ": " ++ s;
  };

let error_recovery = ref(true);

let print_error = (module_name, msg) =>
  Format.eprintf("[Error%s] %s@.", column(module_name), msg);

let error = (module_name: string, default: option('a), msg: string): 'a =>
  switch (error_recovery^, default) {
  | (true, Some(v)) =>
    print_error(module_name, msg);
    v;
  | (false, Some(_))
  | (_, None) => raise([@implicit_arity] Error(module_name, msg))
  };

let warning_message = ref(true);

let warning = (module_name: string, msg: string): unit =>
  if (warning_message^) {
    Format.eprintf("[Warning%s] %s@.", column(module_name), msg);
  };

let debug_message = ref(false);

let debug = (module_name: string, msg: string): unit =>
  if (debug_message^) {
    Format.eprintf("[Debug%s] %s@.", column(module_name), msg);
  };
