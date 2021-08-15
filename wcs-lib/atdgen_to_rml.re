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

let output_line = (ch, s) => {
  output_string(ch, s);
  output_char(ch, '\n');
};

let () = {
  if (Array.length(Sys.argv) != 2) {
    failwith("usage: " ++ Sys.argv[0] ++ " file_t.mli");
  };
  let input_file_name = Sys.argv[1];
  let ouput_file_name =
    if (Filename.check_suffix(input_file_name, ".mli")) {
      Filename.chop_suffix(input_file_name, ".mli") ++ ".rmli";
    } else if (Filename.check_suffix(input_file_name, ".ml")) {
      Filename.chop_suffix(input_file_name, ".ml") ++ ".rml";
    } else {
      failwith("bad suffix");
    };

  let ch_in = open_in(input_file_name);
  let ch_out = open_out(ouput_file_name);
  try({
    let first = input_line(ch_in);
    output_line(ch_out, first);
    let second = input_line(ch_in);
    if (second == "              [@@@ocaml.warning \"-27-32-35-39\"]") {
      ();
    } else {
      output_line(ch_out, second);
    };
    while (true) {
      output_line(ch_out, input_line(ch_in));
    };
  }) {
  | End_of_file =>
    close_in(ch_in);
    close_out(ch_out);
  };
};
