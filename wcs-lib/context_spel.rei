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

/** Json with embedded Spel expressions utilities. */;

/** {8 The ["skip_user_input"] field} */

open Wcs_t;

/** [skip_user_input b] creates the JSON object [{ "skip_user_input" : b }]. */

let skip_user_input: bool => json_spel;

/**
   [set_skip_user_input ctx b] set the field ["skip_user_input"] of
   the object [ctx] with value [b].
*/

let set_skip_user_input: (json_spel, bool) => json_spel;

/**
   [take_skip_user_input ctx] take the field ["skip_user_input"] of
   the object [ctx]. If the field is the defined, it returns [false].
*/

let take_skip_user_input: json_spel => (json_spel, bool);

/** {8 The ["actions"] field} */;

/** [actions acts] creates the JSON object [{ "actions" : acts }]. */

let actions: list(action) => json_spel;

/** [actions_def acts] creates the JSON object [{ "actions" : acts }]. */

let actions_def: list(action_def) => json_spel;

/**
   [set_actions ctx l] set the field ["actions"] of
   the object [ctx] with the list of actions [l].
*/

let set_actions: (json_spel, list(action)) => json_spel;

/**
   [take_actions ctx] take the field ["actions"] of the object
   [ctx].
*/

let take_actions: json_spel => (json_spel, option(list(action)));

/**
   [push_action ctx act] add the action [act] in the list of actions
   stored in the field ["actions"] of ctx. It the field ["actions"]
   doesn't exists, it creates it.
*/

let push_action: (json_spel, action) => json_spel;

/**
   [pop_action ctx] take an action [act] in the list of actions
   stored in the field ["actions"] of ctx.
*/

let pop_action: json_spel => (json_spel, option(action));

/** {8 The ["continuation"] field} */;

/**
   [set_continuation ctx act] set the field ["continuation"] of
   the object [ctx] with the action [act].
*/

let set_continuation: (json_spel, action) => json_spel;

/**
   [get_continuation ctx] get the value of the field ["continuation"]
   of the object [ctx].
*/

let get_continuation: json_spel => option(action);

/**
   [take_continuation ctx] take the value of the field ["continuation"]
   of the object [ctx].
*/

let take_continuation: json_spel => (json_spel, option(action));

/** {8 The ["return"] field} */;

/** [return v] creates the JSON object [{ "return" : v }]. */

let return: json_spel => json_spel;

/**
   [set_return ctx v] set the field ["return"] of
   the object [ctx] with the value [v].
*/

let set_return: (json_spel, json_spel) => json_spel;

/**
   [get_return ctx] get the value of the field ["return"]
   of the object [ctx].
*/

let get_return: json_spel => option(json_spel);
