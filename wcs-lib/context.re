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

let skip_user_input_lbl = "skip_user_input";

let skip_user_input = (b: bool): json =>
  Json.assoc([(skip_user_input_lbl, Json.bool(b))]);

let set_skip_user_input = (ctx: json, b: bool): json =>
  Json.set(ctx, skip_user_input_lbl, `Bool(b));

let take_skip_user_input = (ctx: json): (json, bool) =>
  switch (Json.take(ctx, skip_user_input_lbl)) {
  | (ctx, Some(`Bool(b))) => (ctx, b)
  | _ => (ctx, false)
  };

/** {6. Actions} */;

let actions_lbl = "actions";

let actions = (acts: list(action)): json =>
  Json.assoc([
    (actions_lbl, Json.list(List.map(Wcs.json_of_action, acts))),
  ]);

let yojson_of_action = (act: action): json => Wcs.json_of_action(act);

let action_of_yojson = (act: json): action =>
  Wcs_j.action_of_string(Yojson.Basic.to_string(act));

let set_actions = (ctx, acts: list(action)): json => {
  let js_acts = List.map(yojson_of_action, acts);
  Json.set(ctx, actions_lbl, `List(js_acts));
};

let take_actions = (ctx: json): (json, option(list(action))) =>
  switch (Json.take(ctx, actions_lbl)) {
  | (ctx', Some(`List(acts))) =>
    try((ctx', Some(List.map(action_of_yojson, acts)))) {
    | _ =>
      Log.warning(
        "Context",
        Format.sprintf(
          "illed formed actions:\n%s@.",
          Yojson.Basic.pretty_to_string(`List(acts)),
        ),
      );
      (ctx, None);
    }
  | (_, Some(o)) =>
    Log.warning(
      "Context",
      Format.sprintf(
        "illed formed actions:\n%s@.",
        Yojson.Basic.pretty_to_string(o),
      ),
    );
    (ctx, None);
  | (_, None) => (ctx, None)
  };

let push_action = (ctx: json, act: action): json =>
  switch (take_actions(ctx)) {
  | (ctx, None) => set_actions(ctx, [act])
  | (ctx, Some(acts)) => set_actions(ctx, acts @ [act])
  };

let pop_action = (ctx: json): (json, option(action)) =>
  switch (take_actions(ctx)) {
  | (ctx', Some([act, ...acts])) => (set_actions(ctx', acts), Some(act))
  | _ => (ctx, None)
  };

/** {6. Continuation} */;

let continuation_lbl = "continuation";

let set_continuation = (ctx: json, k: action): json =>
  Json.set(ctx, continuation_lbl, yojson_of_action(k));

let take_continuation = (ctx: json): (json, option(action)) =>
  switch (Json.take(ctx, continuation_lbl)) {
  | (ctx', Some(act)) =>
    try((ctx', Some(action_of_yojson(act)))) {
    | _ =>
      Log.warning(
        "Context",
        Format.sprintf(
          "illed formed continuation:\n%s@.",
          Yojson.Basic.pretty_to_string(act),
        ),
      );
      (ctx, None);
    }
  | _ => (ctx, None)
  };

let get_continuation = (ctx: json): option(action) => {
  let (_, act) = take_continuation(ctx);
  act;
};

/** {6. Return} */;

let return_lbl = "return";

let return = (v: json): json => Json.assoc([(return_lbl, v)]);

let set_return = (ctx: json, x: json): json => Json.set(ctx, "return", x);

let get_return = (ctx: json): option(json) => Json.get(ctx, "return");
