(*
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
 *)

open Wcs_t
module WCS = Wcs
module Spel = Spel

let who_intent =
  WCS.intent "Who"
    ~description: "The user wants to know who is knocking at the door"
    ~examples: [
      "Who's there?";
      "Who is there?";
      "Who are you?";
    ]
    ()

let entity_name =
  WCS.entity "BrokenPencil"
    ~values: ["Broken Pencil", ["Dammaged Pen"; "Fractured Pencil"]]
    ()

let entity_value entity =
  begin match entity.e_def_values with
  | value::_ -> value.e_val_value
  | _ -> "Unknown"
  end

let knock who_intent name_entity answer =
  let knock =
    WCS.dialog_node ("KnockKnock")
      ~conditions_spel: (Spel.bool true)
      ~text: "Knock knock"
      ()
  in
  let whoisthere =
    WCS.dialog_node ("Who")
      ~conditions_spel: (Spel.intent who_intent)
      ~text: (entity_value name_entity)
      ~parent: knock
      ()
  in
  let answer =
    WCS.dialog_node ("Answer")
      ~conditions_spel: (Spel.entity name_entity ())
      ~text: answer
      ~parent: whoisthere
      ~context: (Json.set_skip_user_input `Null true)
      ()
  in
  [knock; whoisthere; answer]

let knockknock =
  WCS.workspace "Knock Knock"
    ~entities: [ entity_name ]
    ~intents: [who_intent]
    ~dialog_nodes: (knock who_intent entity_name "Nevermind it's pointless")
    ()

let main () =
  let wcs_cred_file = ref None in
  let ws_id = ref None in
  let print = ref false in
  let deploy = ref false in
  let exec = ref false in
  let speclist =
    Arg.align
      [ "-cred", Arg.String (fun s -> wcs_cred_file := Some s),
        "cred.json The file containing the Watson Conversation Service credentials.";
        "-id", Arg.String (fun id -> ws_id := Some id),
        "id The workspace id used to update in conjunction with -deploy.";
        "-print", Arg.Set print,
        " Print the workspace on stdout.";
        "-deploy", Arg.Set deploy,
        " Create or update the workspace on Watson Conversation Service.";
        "-exec", Arg.Set exec,
        " Execute the chatbot."
      ]
  in
  let usage =
    "Usage: "^Sys.argv.(0)^" [options]"
  in
  Arg.parse speclist (fun _ -> ()) usage;
  let wcs_cred = Wcs_bot.get_credential !wcs_cred_file in
  begin match !print with
  | true ->
      print_endline (Wcs_json.pretty_workspace knockknock)
  | false ->
      ()
  end;
  begin match !deploy, !ws_id with
  | true, Some ws_id ->
      let () = Wcs_api.update_workspace wcs_cred ws_id knockknock in
      Format.printf "%s: updated@." ws_id
  | true, None ->
      begin match Wcs_api.create_workspace wcs_cred knockknock with
      | { crea_rsp_workspace_id = Some id } ->
          Format.printf "%s: created@." id;
          ws_id := Some id;
      | _ -> assert false
      end
  | false, _ -> ()
  end;
  begin match !exec, !ws_id with
  | true, Some id ->
      let _ = Wcs_bot.exec wcs_cred id `Null "" in
      ()
  | false, _ ->
      ()
  | true, None ->
      Arg.usage speclist "no worksapce to execute";
      exit 1
  end

let _ =
  begin try
    main ()
  with
  | Log.Error (module_name, msg) when not !Log.debug_message ->
      Format.eprintf "%s@." msg;
      exit 1
  end
