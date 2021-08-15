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

/** Context utilities. */;

open Wcs_t;

/** {6. skip_user_input} */;

let skip_user_input = (b: bool): json_spel =>
  Json_spel.assoc([(Context.skip_user_input_lbl, Json_spel.bool(b))]);

let set_skip_user_input = (ctx: json_spel, b: bool): json_spel =>
  Json_spel.set(ctx, Context.skip_user_input_lbl, `Bool(b));

let take_skip_user_input = (ctx: json_spel): (json_spel, bool) =>
  switch (Json_spel.take(ctx, Context.skip_user_input_lbl)) {
  | (ctx, Some(`Bool(b))) => (ctx, b)
  | _ => (ctx, false)
  };

/** {6. Actions} */;

let actions = (acts: list(action)): json_spel =>
  Json_spel.assoc([
    (
      Context.actions_lbl,
      Json_spel.list(List.map(Wcs.json_spel_of_action, acts)),
    ),
  ]);

let actions_def = (acts: list(action_def)): json_spel =>
  Json_spel.assoc([
    (
      Context.actions_lbl,
      Json_spel.list(List.map(Wcs.json_spel_of_action_def, acts)),
    ),
  ]);

let json_spel_of_action = (act: action): json_spel => {
  let json = Wcs.json_of_action(act);
  let json_spel = Json_spel.of_json(json);
  json_spel;
};

let action_of_json_spel = (act: json_spel): action =>
  Wcs_j.action_of_string(Yojson.Basic.to_string(Json_spel.to_json(act)));

let set_actions = (ctx, acts: list(action)): json_spel => {
  let js_acts = List.map(json_spel_of_action, acts);
  Json_spel.set(ctx, Context.actions_lbl, `List(js_acts));
};

let take_actions = (ctx: json_spel): (json_spel, option(list(action))) =>
  switch (Json_spel.take(ctx, Context.actions_lbl)) {
  | (ctx', Some(`List(acts))) =>
    try((ctx', Some(List.map(action_of_json_spel, acts)))) {
    | _ =>
      Log.warning(
        "Context_spel",
        Format.sprintf(
          "illed formed actions:\n%s@.",
          Yojson.Basic.pretty_to_string(Json_spel.to_json(`List(acts))),
        ),
      );
      (ctx, None);
    }
  | (_, Some(o)) =>
    Log.warning(
      "Context_spel",
      Format.sprintf(
        "illed formed actions:\n%s@.",
        Yojson.Basic.pretty_to_string(Json_spel.to_json(o)),
      ),
    );
    (ctx, None);
  | (_, None) => (ctx, None)
  };

let push_action = (ctx: json_spel, act: action): json_spel =>
  switch (take_actions(ctx)) {
  | (ctx, None) => set_actions(ctx, [act])
  | (ctx, Some(acts)) => set_actions(ctx, acts @ [act])
  };

let pop_action = (ctx: json_spel): (json_spel, option(action)) =>
  switch (take_actions(ctx)) {
  | (ctx', Some([act, ...acts])) => (set_actions(ctx', acts), Some(act))
  | _ => (ctx, None)
  };

/** {6. Continuation} */;

let set_continuation = (ctx: json_spel, k: action): json_spel =>
  Json_spel.set(ctx, Context.continuation_lbl, json_spel_of_action(k));

let take_continuation = (ctx: json_spel): (json_spel, option(action)) =>
  switch (Json_spel.take(ctx, Context.continuation_lbl)) {
  | (ctx', Some(act)) =>
    try((ctx', Some(action_of_json_spel(act)))) {
    | _ =>
      Log.warning(
        "Context_spel",
        Format.sprintf(
          "illed formed continuation:\n%s@.",
          Yojson.Basic.pretty_to_string(Json_spel.to_json(act)),
        ),
      );
      (ctx, None);
    }
  | _ => (ctx, None)
  };

let get_continuation = (ctx: json_spel): option(action) => {
  let (_, act) = take_continuation(ctx);
  act;
};

/** {6. Return} */;

let return = (v: json_spel): json_spel =>
  Json_spel.assoc([(Context.return_lbl, v)]);

let set_return = (ctx: json_spel, x: json_spel): json_spel =>
  Json_spel.set(ctx, Context.return_lbl, x);

let get_return = (ctx: json_spel): option(json_spel) =>
  Json_spel.get(ctx, Context.return_lbl);
