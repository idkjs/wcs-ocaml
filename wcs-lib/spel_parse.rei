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

/** Spel parsers. */;

/** {6 desugaring} */
/** Set if builder should desugar spel expressions */

let desugar_spel: ref(bool);

/** [desugar expr] expands shorthand syntax for variables, entities
    and intents into their underlying Spel expressions. */

let desugar: Spel_t.expression => Spel_t.expression;

/** Set if builder should resugar spel expressions */

let resugar_spel: ref(bool);

/** [desugar expr] re-introduces shorthand syntax for variables,
    entities and intents. */

let resugar: Spel_t.expression => Spel_t.expression;

/** {6 parsers} */;

/** [expr_from_file f] parses file [f] as spel expression */

let expr_from_file: string => Spel_t.expression;

/** [expr_from_string s] parses string [s] as spel expression */

let expr_from_string: string => Spel_t.expression;

/** [quoted_expr_from_file f] parses file [f] as text containing
    quoted spel expressions */

let quoted_expr_from_file: string => Spel_t.expression;

/** [quoted_expr_from_string f] parses string [s] as text containing
    quoted spel expression */

let quoted_expr_from_string: string => Spel_t.expression;
