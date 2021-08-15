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

open Wcs_t;

module Make =
       (
         Wcs_call: {
           let message:
             (credential, string, message_request) => message_response;
         },
       ) => {
  let pretty_json_string = s =>
    Yojson.Basic.pretty_to_string(Yojson.Basic.from_string(s));

  let before_default = (msg_req: message_request): message_request => msg_req;

  let after_default = (msg_resp: message_response): message_response => msg_resp;

  let user_input_default = () => {
    print_string("H: ");
    flush(stdout);
    input_line(stdin);
  };

  let output_default = txt => {
    print_string("C: ");
    print_endline(txt);
  };

  let interpret =
      (~before=before_default, ~after=after_default, wcs_cred: credential)
      : (
          (string, message_request) =>
          (string, message_response, option(json))
        ) => {
    let rec interpret =
            (ws_id: string, req_msg: message_request)
            : (string, message_response, option(json)) => {
      let req_msg = before(req_msg);
      Log.debug(
        "Wcs_bot",
        "Request:\n" ++ Wcs_pretty.message_request(req_msg),
      );
      let resp = Wcs_call.message(wcs_cred, ws_id, req_msg);

      Log.debug(
        "Wcs_bot",
        "Response:\n" ++ Wcs_pretty.message_response(resp),
      );
      let resp = after(resp);
      let ctx = resp.msg_rsp_context;
      let output = resp.msg_rsp_output;
      switch (Context.take_actions(ctx)) {
      | (ctx, Some([act])) =>
        let k = {
          act_name: ws_id,
          act_agent: "client",
          act_type_: "conversation",
          act_parameters: Json.assoc([("context", ctx)]),
          act_result_variable: act.act_result_variable,
        };

        let act_ctx =
          switch (Json.get(act.act_parameters, "context")) {
          | None => `Null
          | Some(ctx) => ctx
          };

        let act_parameters =
          Json.set(
            act.act_parameters,
            "context",
            Context.set_continuation(act_ctx, k),
          );

        let act = {...act, act_parameters};
        interpret_action(act, output);
      | (ctx, Some([_, _, ..._])) => assert(false) /* XXX TODO XXX */
      | (ctx, Some([]))
      | (ctx, None) =>
        let (ctx, skip_user_input) = Context.take_skip_user_input(ctx);
        switch (Context.get_return(ctx)) {
        | Some(v) =>
          switch (Context.get_continuation(ctx)) {
          | Some(k) =>
            let k_txt =
              if (skip_user_input) {
                req_msg.msg_req_input.in_text;
              } else {
                "";
              };

            let k_ctx =
              switch (Json.get(k.act_parameters, "context")) {
              | Some(ctx) => ctx
              | None => `Null
              };

            let k_ctx =
              switch (k.act_result_variable) {
              | None => k_ctx
              | Some(lbl) =>
                let prefix = String.sub(lbl, 0, 8);
                let var = String.sub(lbl, 8, String.length(lbl) - 8);
                assert(prefix == "context.");
                Json.set(k_ctx, var, v);
              };

            let (k_ctx, k_skip_user_input) =
              Context.take_skip_user_input(k_ctx);

            if (k_skip_user_input) {
              let k_parameters =
                Json.assign([
                  k.act_parameters,
                  Json.assoc([("text", Json.string(k_txt))]),
                  Json.assoc([("context", k_ctx)]),
                ]);

              let k = {...k, act_parameters: k_parameters};
              interpret_action(k, output);
            } else {
              let k_resp = {...resp, msg_rsp_context: k_ctx};

              (k.act_name, k_resp, Context.get_return(k_ctx));
            };
          | None => (ws_id, resp, Some(v))
          }
        | None =>
          if (skip_user_input) {
            interpret(
              ws_id,
              {
                ...req_msg,
                msg_req_context: Some(ctx),
                msg_req_output: Some(output),
              },
            );
          } else {
            (ws_id, {...resp, msg_rsp_context: ctx}, None);
          }
        };
      };
    }

    and interpret_action = (act, output) =>
      switch (act.act_agent, act.act_type_) {
      | ("client", "conversation") =>
        let ctx =
          switch (Json.get(act.act_parameters, "context")) {
          | None => `Null
          | Some(ctx) => ctx
          };

        let txt =
          switch (Json.get_string(act.act_parameters, "text")) {
          | None => ""
          | Some(s) => s
          };

        let req_msg = {
          msg_req_input: {
            in_text: txt,
          },
          msg_req_alternate_intents: false,
          msg_req_context: Some(ctx),
          msg_req_entities: None,
          msg_req_intents: None,
          msg_req_output: Some(output),
        };

        interpret(act.act_name, req_msg);
      | _ => assert(false)
      };

    interpret;
  };

  let exec =
      (
        ~before=before_default,
        ~after=after_default,
        ~user_input=user_input_default,
        ~output=output_default,
        wcs_cred: credential,
        workspace_id: string,
        ctx_init: json,
        txt_init: string,
      )
      : json => {
    let interpret = interpret(~before, ~after, wcs_cred);

    let rec loop = (ws_id, ctx, txt) => {
      let req = {
        msg_req_input: {
          in_text: txt,
        },
        msg_req_alternate_intents: false,
        msg_req_context: Some(ctx),
        msg_req_entities: None,
        msg_req_intents: None,
        msg_req_output: None,
      };

      let (ws_id, rsp, return) = interpret(ws_id, req);
      List.iter(output, rsp.msg_rsp_output.out_text);
      switch (return) {
      | Some(v) => v
      | None =>
        let txt = user_input();
        let ctx = rsp.msg_rsp_context;
        loop(ws_id, ctx, txt);
      };
    };

    loop(workspace_id, ctx_init, txt_init);
  };

  let get_credential = file_name_opt =>
    try({
      let file_name =
        switch (file_name_opt) {
        | Some(file_name) => file_name
        | None => Sys.getenv("WCS_CRED")
        };

      Json.read_json_file(Wcs_j.read_credential, file_name);
    }) {
    | Not_found => Log.error("Wcs_bot", None, "no credential file")
    | exn => Log.error("Wcs_bot", None, Printexc.to_string(exn))
    };
};
