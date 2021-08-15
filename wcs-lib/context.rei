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

/** Context utilities. */

open Wcs_t;

/** {8 The ["skip_user_input"] field} */;

/** The ["skip_user_input"] string. */

let skip_user_input_lbl: string;

/** [skip_user_input b] creates the JSON object [{ "skip_user_input" : b }]. */

let skip_user_input: bool => json;

/**
   [set_skip_user_input ctx b] set the field ["skip_user_input"] of
   the object [ctx] with value [b].
*/

let set_skip_user_input: (json, bool) => json;

/**
   [take_skip_user_input ctx] take the field ["skip_user_input"] of
   the object [ctx]. If the field is the defined, it returns [false].
*/

let take_skip_user_input: json => (json, bool);

/** {8 The ["actions"] field} */;

/** The ["actions"] string. */

let actions_lbl: string;

/** [actions acts] creates the JSON object [{ "actions" : acts }]. */

let actions: list(action) => json;

/**
   [set_actions ctx l] set the field ["actions"] of
   the object [ctx] with the list of actions [l].
*/

let set_actions: (json, list(action)) => json;

/**
   [take_actions ctx] take the field ["actions"] of the object
   [ctx].
*/

let take_actions: json => (json, option(list(action)));

/**
   [push_action ctx act] add the action [act] in the list of actions
   stored in the field ["actions"] of ctx. It the field ["actions"]
   doesn't exists, it creates it.
*/

let push_action: (json, action) => json;

/**
   [pop_action ctx] take an action [act] in the list of actions
   stored in the field ["actions"] of ctx.
*/

let pop_action: json => (json, option(action));

/** {8 The ["continuation"] field} */;

/** The ["continuation"] string. */

let continuation_lbl: string;

/**
   [set_continuation ctx act] set the field ["continuation"] of
   the object [ctx] with the action [act].
*/

let set_continuation: (json, action) => json;

/**
   [get_continuation ctx] get the value of the field ["continuation"]
   of the object [ctx].
*/

let get_continuation: json => option(action);

/**
   [take_continuation ctx] take the value of the field ["continuation"]
   of the object [ctx].
*/

let take_continuation: json => (json, option(action));

/** {8 The ["return"] field} */;

/** The ["return"] string. */

let return_lbl: string;

/** [return v] creates the JSON object [{ "return" : v }]. */

let return: json => json;

/**
   [set_return ctx v] set the field ["return"] of
   the object [ctx] with the value [v].
*/

let set_return: (json, json) => json;

/**
   [get_return ctx] get the value of the field ["return"]
   of the object [ctx].
*/

let get_return: json => option(json);
