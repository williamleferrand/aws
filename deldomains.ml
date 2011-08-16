(* command line tool for carefully deleteing AWS SimpleDB domains *)

open Lwt
open Lwt_io

module StringMap = Map.Make(String)

let main () =
  let creds = Aws_util.creds_of_env () in
  lwt domains = Dbi_util.list_domains creds >>= function 
    | `Error (a,b) -> 
        printf "list domains failure: %s %s\n%!" a b >>
          exit 1
    | `Ok list -> return list
  in
  printf "found %d domains\n%!" (List.length domains) >>
    lwt _, map = Lwt_list.fold_left_s (
      fun (count, map) domain ->
        printf "%d. %s\n%!" count domain >> 
          let count_s = string_of_int count in 
          let map' = StringMap.add count_s domain map in
          return (count + 1, map')
    ) (1, StringMap.empty) domains in
    printf "which domain(s) to delete? (space-separated index)\n>> %!" >>
    lwt line = read_line stdin in
    let domain_indexes_to_delete = Pcre.split line in
    printf "go ahead and delete these domains:\n%!" >>
      lwt domains_to_delete = Lwt_list.fold_left_s (
        fun domains_to_delete domain_index ->
          try
            let domain = StringMap.find domain_index map in
            printf "%s. %s\n" domain_index domain >>
              return (domain :: domains_to_delete)
          with Not_found ->
            printf "%s. --- not found ---\n" domain_index >>
              return domains_to_delete
      ) [] domain_indexes_to_delete in
      let domains_to_delete = List.rev domains_to_delete in
      let rec loop_until_y_or_n () =
        printf "? [y/n] " >>
          read_line stdin >>=function
            | "y" -> return true
            | "n" -> return false
            | _ -> loop_until_y_or_n ()
      in
      loop_until_y_or_n () >>= function
        | false -> printf "aborting\n%!"
        | true ->
            Lwt_list.iter_s (
              fun domain ->
                printf "deleting %s ... %!" domain >>
                SDB.delete_domain creds domain >>= function
                  | `Ok -> printf "ok\n%!"
                  | `Error (a, b) ->
                      printf "delete domain failure: %s %s" a b >>
                        exit 1
            ) domains_to_delete >>
              return ()

let _ =
  Lwt_unix.run (main ())

(*

Copyright (c) 2011, barko 00336ea19fcb53de187740c490f764f4
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
      
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of barko nor the names of contributors may be used
   to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*)
