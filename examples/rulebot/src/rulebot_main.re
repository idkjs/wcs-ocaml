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
open Cnl_util;
open Cnl_print;

open Bmd_t;
open Bmd_util;
open Bmd_samples;
open Bmd_to_wcs_entities;
open Bmd_spec_to_bmd;

open Wcs_t;
open Dialog_interface_t;

/* Util */

let oiter = (f, o) =>
  switch (o) {
  | None => ()
  | Some(x) => f(x)
  };

let read_json_file = (filetype, reader, f) =>
  try({
    let lexstate = Yojson.init_lexer(~fname=f, ());
    let ch = open_in(f);
    let lexbuf = Lexing.from_channel(ch);
    let json = reader(lexstate, lexbuf);
    close_in(ch);
    json;
  }) {
  | Yojson.Json_error(err) =>
    raise(
      Failure("Unable to parse " ++ filetype ++ " file " ++ f ++ ": " ++ err),
    )
  | exn =>
    raise(
      Failure(
        "Unable to read "
        ++ filetype
        ++ " file "
        ++ f
        ++ ": "
        ++ Printexc.to_string(exn),
      ),
    )
  };

let load_io = io_file => read_json_file("io", Io_j.read_io, io_file);

let convert_json_rule = j =>
  switch (Cnl_t.cnl_rule_of_yojson(j)) {
  | Deriving_intf.Ok(x) => x
  | Deriving_intf.Error(_) => raise(Failure("Couldn't parse json file"))
  };

let convert_json_instr = j =>
  switch (Cnl_instr_t.cnl_instr_of_yojson(j)) {
  | Deriving_intf.Ok(x) => x
  | Deriving_intf.Error(_) => raise(Failure("Couldn't parse json file"))
  };

let parse_rule = f => {
  let rule_j = Yojson.Safe.from_file(f);
  convert_json_rule(rule_j);
};

let get_workspace_id =
    (~update=false, ws_name, wcs_cred, ws_config, get_id_fname, ws) => {
  let (ws_id, ws_fname) =
    switch (ws_config) {
    | Some(config) => get_id_fname(config)
    | None => (None, None)
    };

  switch (ws_id) {
  | Some(id) =>
    switch (update, ws) {
    | (true, Some(w)) =>
      Wcs_call.update_workspace(wcs_cred, id, w);
      id;
    | (_, _) => id
    }
  | None =>
    let workspace =
      switch (ws_fname, ws) {
      | (Some(fname), _) =>
        read_json_file("workspace", Wcs_j.read_workspace, fname)
      | (None, Some(w)) => w
      | (None, None) =>
        raise(Failure("Workspace " ++ ws_name ++ " required"))
      };

    let rsp = Wcs_call.create_workspace(wcs_cred, workspace);
    switch (rsp.crea_rsp_workspace_id) {
    | Some(id) => id
    | None => raise(Failure("Unable to create workspace " ++ ws_name))
    };
  };
};

let string_of_ws_ids = ws_ids => {
  let config = {
    ws_select_example: None,
    ws_select_example_id: None,
    ws_select_expr: None,
    ws_select_expr_id: None,
    ws_dispatch: None,
    ws_dispatch_id: Some(ws_ids.Dialog_util.ws_dispatch_id),
    ws_when: None,
    ws_when_id: Some(ws_ids.Dialog_util.ws_when_id),
    ws_cond: None,
    ws_cond_id: Some(ws_ids.Dialog_util.ws_cond_id),
    ws_cond_continue: None,
    ws_cond_continue_id: Some(ws_ids.Dialog_util.ws_cond_continue_id),
    ws_then: None,
    ws_then_id: Some(ws_ids.Dialog_util.ws_then_id),
    ws_expr: None,
    ws_expr_id: Some(ws_ids.Dialog_util.ws_expr_id),
    ws_actn: None,
    ws_actn_id: Some(ws_ids.Dialog_util.ws_actn_id),
    ws_accept: None,
    ws_accept_id: Some(ws_ids.Dialog_util.ws_accept_id),
  };

  Dialog_interface_j.string_of_config(config);
};

let load_ws_ids = (wcs_cred, workspaces_config, ws_update, bmd) => {
  let ws_dispatch_id =
    get_workspace_id(
      ~update=ws_update,
      "dispatch",
      wcs_cred,
      workspaces_config,
      config => (config.ws_dispatch_id, config.ws_dispatch),
      Some(Ws_dispatch.ws_dispatch(fst(bmd))),
    );

  let ws_when_id =
    get_workspace_id(
      ~update=ws_update,
      "when",
      wcs_cred,
      workspaces_config,
      config => (config.ws_when_id, config.ws_when),
      Some(Ws_when.ws_when(snd(bmd))),
    );

  let ws_cond_id =
    get_workspace_id(
      ~update=ws_update,
      "cond",
      wcs_cred,
      workspaces_config,
      config => (config.ws_cond_id, config.ws_cond),
      Some(Ws_cond.ws_cond),
    );

  let ws_cond_continue_id =
    get_workspace_id(
      ~update=ws_update,
      "cond-continue",
      wcs_cred,
      workspaces_config,
      config => (config.ws_cond_continue_id, config.ws_cond_continue_id),
      Some(Ws_cond_continue.ws_cond_continue),
    );

  let ws_then_id =
    get_workspace_id(
      ~update=ws_update,
      "then",
      wcs_cred,
      workspaces_config,
      config => (config.ws_then_id, config.ws_then),
      Some(Ws_then.ws_then),
    );

  let ws_expr_id =
    get_workspace_id(
      ~update=ws_update,
      "expr",
      wcs_cred,
      workspaces_config,
      config => (config.ws_expr_id, config.ws_expr),
      Some(Ws_expr.ws_expr(snd(bmd))),
    );

  let ws_actn_id =
    get_workspace_id(
      ~update=ws_update,
      "actn",
      wcs_cred,
      workspaces_config,
      config => (config.ws_actn_id, config.ws_actn),
      Some(Ws_actn.ws_actn(snd(bmd))),
    );

  let ws_accept_id =
    get_workspace_id(
      ~update=ws_update,
      "accept",
      wcs_cred,
      workspaces_config,
      config => (config.ws_accept_id, config.ws_accept),
      Some(Ws_accept.ws_accept),
    );

  let ws_ids = {
    Dialog_util.ws_dispatch_id,
    ws_when_id,
    ws_cond_id,
    ws_cond_continue_id,
    ws_then_id,
    ws_expr_id,
    ws_actn_id,
    ws_accept_id,
  };

  Io_util.print_workspace(string_of_ws_ids(ws_ids));
  ws_ids;
};

/* Command line */
type mode =
  | M_nothing
  | M_bot
  | M_ws_gen
  | M_ws_delete;

let rulebot_mode: ref(mode) = (ref(M_bot): ref(mode));
let set_ws_delete_mode = f => rulebot_mode := M_ws_delete;
let set_ws_gen_mode = f => rulebot_mode := M_ws_gen;

let bom_io = ref(None);
let set_bom_io = file => bom_io := Some(file);

let bmd = ref(None);
let set_bmd = file => bmd := Some(file);

let wcs_credential: ref(option(string)) = (
  ref(None): ref(option(string))
);
let set_wcs_credential = f => wcs_credential := Some(f);

let workspaces_config = ref(None);
let set_workspaces_config = f => {
  let config =
    read_json_file("workspaces_config", Dialog_interface_j.read_config, f);

  workspaces_config := Some(config);
};

let ws_update = ref(false);
let set_ws_update = () => ws_update := true;

let is_slack = ref(false);
let set_is_slack = () => is_slack := true;

let slackbot = ref("cat");
let set_slackbot = cmd => slackbot := cmd;

let rule_init: ref(cnl_rule) = (
  ref(Cnl_samples.rule_init()): ref(cnl_rule)
);
let set_init_rule = f => Cnl_samples.set_rule_init(parse_rule(f));

let anon_args = f => raise(Failure("rulebot expects no parameters"));

let args =
  Arg.align([
    (
      "-wcs-cred",
      Arg.String(set_wcs_credential),
      "file The file containing the Watson Conversation Service credentials",
    ),
    (
      "-bom-io",
      Arg.String(set_bom_io),
      "io.json replace workspace entities using I/O file",
    ),
    (
      "-bmd",
      Arg.String(set_bmd),
      ".bmd replace workspace entities using BMD file",
    ),
    (
      "-ws-config",
      Arg.String(set_workspaces_config),
      "config.json The file containing id or file name of the workspaces",
    ),
    (
      "-ws-update",
      Arg.Unit(set_ws_update),
      " Updates workspaces given by -workspaces-config in Watson Conversation (the others are deployed)",
    ),
    (
      "-ws-delete",
      Arg.Unit(set_ws_delete_mode),
      " Delete workspaces given by -workspaces-config in Watson Conversation",
    ),
    ("-ws-gen", Arg.Unit(set_ws_gen_mode), " Generate workspaces in JSON"),
    ("-slack", Arg.Unit(set_is_slack), " Launch as a Slackbot"),
    ("-slack-io", Arg.Unit(Io_util.set_slack_io), " I/O for Slackbot"),
    (
      "-slack-log",
      Arg.String(Io_util.set_slack_log),
      " Log I/O for Slackbot",
    ),
    (
      "-slackbot",
      Arg.String(set_slackbot),
      "cmd Set the slackbot-stdio command",
    ),
    ("-rule", Arg.String(set_init_rule), "r Set an initial rule"),
  ]);

let usage = "rulebot [options]";

/* Workspaces generation */
let workspaces_generation = bmd => {
  let output_ws = ws => {
    let ws_s = Io_util.pretty_json_string(Wcs_j.string_of_workspace(ws));

    switch (ws.ws_name) {
    | Some(name) =>
      let fname = name ++ ".json";
      print_endline(fname ++ " generated");
      let ch = open_out(fname);
      output_string(ch, ws_s);
      close_out(ch);
    | None => print_endline(ws_s)
    };
  };

  output_ws(Ws_accept.ws_accept);
  output_ws(Ws_actn.ws_actn(snd(bmd)));
  output_ws(Ws_then.ws_then);
  output_ws(Ws_expr.ws_expr(snd(bmd)));
  output_ws(Ws_dispatch.ws_dispatch(fst(bmd)));
  output_ws(Ws_cond.ws_cond);
  output_ws(Ws_cond_continue.ws_cond_continue);
  output_ws(Ws_when.ws_when(snd(bmd)));
  ();
};

/* Workspace deletion */

let workspaces_delete = wcs_cred => {
  let ws_conf =
    switch (workspaces_config^) {
    | Some(conf) => conf
    | None => raise(Failure("Workspaces configuration file required"))
    };

  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_select_example_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_select_expr_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_dispatch_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_when_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_cond_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_cond_continue_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_then_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_expr_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_actn_id);
  oiter(Wcs_call.delete_workspace(wcs_cred), ws_conf.ws_accept_id);
  ();
};
