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

module Make = (Client: Cohttp_lwt.S.Client) => {
  open Lwt;
  open Wcs_t;
  let version = "version=2017-05-26";

  /** {6. Check workspaces} */;

  let ws_check = ws => {
    let check_node_names = nodes => {
      let tbl = Hashtbl.create(7);
      List.iter(
        node => {
          if (Hashtbl.mem(tbl, node.node_dialog_node)) {
            Log.error(
              "Wcs",
              Some(),
              "Multiple nodes with name " ++ node.node_dialog_node,
            );
          };
          Hashtbl.add(tbl, node.node_dialog_node, true);
        },
        nodes,
      );
    };

    check_node_names(ws.ws_dialog_nodes);
    true;
  };

  /** {6. Utility functions} */;

  let parameters_of_json = (o: json): string =>
    switch (o) {
    | `Assoc([]) => ""
    /* | `Assoc ((x, v) :: l) -> */
    /*     let params = "?"^x^"="^(Yojson.Basic.to_string o) in */
    /*     List.fold_left */
    /*       (fun params (x, v) -> */
    /*         "&"^x^"="^(Yojson.Basic.to_string o)) */
    /*       params l */
    | `Assoc(l) =>
      List.fold_left(
        (params, (x, v)) =>
          switch (v) {
          | `String(s) => "&" ++ x ++ "=" ++ s
          | _ => "&" ++ x ++ "=" ++ Yojson.Basic.to_string(o)
          },
        "",
        l,
      )
    | _ =>
      Log.error(
        "Wcs",
        Some(""),
        "parameters_of_json "
        ++ Yojson.Basic.pretty_to_string(o)
        ++ ": json object expected",
      )
    };

  /** {6. Generic functions} */;

  let post = (wcs_cred, method, req) => {
    let uri = Uri.of_string(wcs_cred.cred_url ++ method ++ "?" ++ version);

    let headers = {
      let h = Cohttp.Header.init();
      let h =
        Cohttp.Header.add_authorization(
          h,
          `Basic((wcs_cred.cred_username, wcs_cred.cred_password)),
        );
      let h = Cohttp.Header.add(h, "Content-Type", "application/json");
      h;
    };

    let data = (Cohttp.Body.of_string(req) :> Cohttp_lwt.Body.t);
    let call =
      Client.post(~body=data, ~headers, uri)
      >>= (
        ((resp, body)) => {
          let code =
            resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status;
          body
          |> Cohttp_lwt.Body.to_string
          >|= (
            body =>
              switch (code) {
              | 200
              | 201 => body
              | _ =>
                Log.error(
                  "Wcs",
                  None,
                  Format.sprintf("[POST %s] %d: %s", method, code, body),
                )
              }
          );
        }
      );

    let rsp = Lwt_main.run(call);
    rsp;
  };

  let get = (wcs_cred, method, params) => {
    let uri =
      Uri.of_string(wcs_cred.cred_url ++ method ++ "?" ++ version ++ params);

    let headers = {
      let h = Cohttp.Header.init();
      let h =
        Cohttp.Header.add_authorization(
          h,
          `Basic((wcs_cred.cred_username, wcs_cred.cred_password)),
        );
      let h = Cohttp.Header.add(h, "Content-Type", "application/json");
      h;
    };

    let call =
      Client.get(~headers, uri)
      >>= (
        ((resp, body)) => {
          let code =
            resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status;
          body
          |> Cohttp_lwt.Body.to_string
          >|= (
            body =>
              switch (code) {
              | 200 => body
              | _ =>
                Log.error(
                  "Wcs",
                  None,
                  Format.sprintf("[GET %s] %d: %s", method, code, body),
                )
              }
          );
        }
      );

    let rsp = Lwt_main.run(call);
    rsp;
  };

  let delete = (wcs_cred, method) => {
    let uri = Uri.of_string(wcs_cred.cred_url ++ method ++ "?" ++ version);

    let headers = {
      let h = Cohttp.Header.init();
      let h =
        Cohttp.Header.add_authorization(
          h,
          `Basic((wcs_cred.cred_username, wcs_cred.cred_password)),
        );
      let h = Cohttp.Header.add(h, "Content-Type", "application/json");
      h;
    };

    let call =
      Client.delete(~headers, uri)
      >>= (
        ((resp, body)) => {
          let code =
            resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status;
          body
          |> Cohttp_lwt.Body.to_string
          >|= (
            body =>
              switch (code) {
              | 200
              | 201 => body
              | _ =>
                Log.error(
                  "Wcs",
                  None,
                  Format.sprintf("[DELETE %s] %d: %s", method, code, body),
                )
              }
          );
        }
      );

    let rsp = Lwt_main.run(call);
    rsp;
  };

  /** {Watson Conversation API} */;

  let list_workspaces = (wcs_cred, req) => {
    let method = "/v1/workspaces";
    let params =
      parameters_of_json(Wcs.json_of_list_workspaces_request(req));

    let rsp = get(wcs_cred, method, params);
    Wcs_j.list_workspaces_response_of_string(rsp);
  };

  let create_workspace = (wcs_cred, workspace) => {
    assert(ws_check(workspace));
    let method = "/v1/workspaces";
    let req = Wcs_j.string_of_workspace(workspace);
    let rsp =
      try(post(wcs_cred, method, req)) {
      | [@implicit_arity] Log.Error("Wcs", err) =>
        switch (workspace.ws_name) {
        | Some(ws_name) =>
          Log.error("Wcs", None, Format.sprintf("[%s]%s", ws_name, err))
        | None => Log.error("Wcs", None, err)
        }
      };

    Wcs_j.create_response_of_string(rsp);
  };

  let delete_workspace = (wcs_cred, workspace_id) => {
    let method = "/v1/workspaces/" ++ workspace_id;
    let rsp = delete(wcs_cred, method);
    ignore(rsp);
  };

  let get_workspace = (wcs_cred, req) => {
    let method = "/v1/workspaces/" ++ req.get_ws_req_workspace_id;
    let params =
      switch (req.get_ws_req_export) {
      | None => ""
      | Some(b) => "&export=" ++ string_of_bool(b)
      };

    let rsp = get(wcs_cred, method, params);
    Wcs_j.workspace_of_string(rsp);
  };

  let update_workspace = (wcs_cred, workspace_id, workspace) => {
    assert(ws_check(workspace));
    let method = "/v1/workspaces/" ++ workspace_id;
    let req = Wcs_j.string_of_workspace(workspace);
    let rsp =
      try(post(wcs_cred, method, req)) {
      | [@implicit_arity] Log.Error("Wcs", err) =>
        switch (workspace.ws_name) {
        | Some(ws_name) =>
          Log.error("Wcs", None, Format.sprintf("[%s]%s", ws_name, err))
        | None => Log.error("Wcs", None, err)
        }
      };

    ignore(rsp);
  };

  let message = (wcs_cred, workspace_id, req_msg) => {
    let method = "/v1/workspaces/" ++ workspace_id ++ "/message";
    let req = Wcs_j.string_of_message_request(req_msg);
    let rsp = post(wcs_cred, method, req);
    Wcs_j.message_response_of_string(rsp);
  };

  /** {6 Logs} */;

  let logs = (wcs_cred, workspace_id, req) => {
    let method = "/v1/workspaces/" ++ workspace_id ++ "/logs";
    let params = parameters_of_json(Wcs.json_of_logs_request(req));

    let rsp = get(wcs_cred, method, params);
    Wcs_j.logs_response_of_string(rsp);
  };
};
