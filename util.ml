(* Copyright (c) 2010, barko 00336ea19fcb53de187740c490f764f4 All
   rights reserved.

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


let remove_newline = 
  Pcre.replace ~rex:(Pcre.regexp "\n") ~templ:""

let base64 str =
  (* the encoder is consumed by its use, so we have to recreated *)
  let b64_encoder = Cryptokit.Base64.encode_multiline () in
  let encoded = Cryptokit.transform_string b64_encoder str in

  (* we want to retain the trailing '=' characters, but eliminate the
     newlines.  Unfortunately, [encode_compact] has neither. *)
  remove_newline encoded

let colon_space (k, v) = k ^ ": " ^ v

let encode_url s = Netencoding.Url.encode s

let encode_key_equals_value kvs = 
  List.map (
    fun (k,v) -> 
      (encode_url k) ^ "=" ^ (encode_url v)
  ) kvs


let file_size path =
  let s = Unix.stat path in
  s.Unix.st_size

let xml_declaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

let string_of_xml = Xml.to_string 

let sort_assoc_list kv_list = 
  List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) kv_list
  
let getenv_else_exit k = 
  try 
    Unix.getenv k
  with Not_found ->
    failwith (Printf.sprintf "environment variable %S not set\n%!" k)

open Creds
let creds_of_env () = {
  aws_access_key_id = getenv_else_exit "AWS_ACCESS_KEY_ID";
  aws_secret_access_key = getenv_else_exit "AWS_SECRET_ACCESS_KEY"
}
  
