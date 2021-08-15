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

let string_of_file: string => string;

let parse: (('a, Lexing.lexbuf) => 'b, 'a, Lexing.lexbuf) => 'b;
let parse_string: (Lexing.lexbuf => 'a, string) => 'a;
let parse_file: (Lexing.lexbuf => 'a, string) => 'a;
let parse_expr: Lexing.lexbuf => Cnl_t.cnl_expr;
let parse_cnl_expr_from_string: string => Cnl_t.cnl_expr;
let parse_rule: Lexing.lexbuf => Cnl_t.cnl_rule;
let parse_cnl_rule_from_string: string => Cnl_t.cnl_rule;
let parse_bmd_spec: Lexing.lexbuf => Bmd_t.bmd_spec;
let parse_bmd_spec_from_string: string => Bmd_t.bmd_spec;
let parse_bmd_spec_from_file: string => Bmd_t.bmd_spec;
