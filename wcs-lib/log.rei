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

/**
   Logging utilities.
*/;

/** {6 Errors} */;

/** Exception raise in case of error. It is associated with the module
    name and the error message.
*/

exception Error(string, string);

/** Set if we should avoid to fail in case of error. */

let error_recovery: ref(bool);

/** [error module_name default msg] raises [Error]. If a default value
    is provided and [error_recovery] is [true], it returns the value
    instead of raising the exception [Error]. In this case, the error
    message is printed.
*/

let error: (string, option('a), string) => 'a;

/** [print_error module_name msg] prints the error message [msg]
    prefixed with the module name [module_name].
*/

let print_error: (string, string) => unit;

/** {6 Warnings} */;

/** Set if we should display warnings. */

let warning_message: ref(bool);

/** [warning module_name msg] prints the warning message [msg] prefixed
    with the module name [module_name].
*/

let warning: (string, string) => unit;

/** {6 Debug messages} */;

/** Set if we should display debug. */

let debug_message: ref(bool);

/** [debug module_name msg] prints the debug message [msg] prefixed
    with the module name [module_name].
*/

let debug: (string, string) => unit;
