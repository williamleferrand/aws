(** command-line client to Amazon's S3 *)

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

open Lwt
open Creds
open Printf

let create_bucket creds bucket () =
  lwt result = S3.create_bucket creds bucket `Private in
  let exit_code = 
    match result with
      | `Ok -> print_endline "ok"; 0
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let delete_bucket creds bucket () =
  lwt result = S3.delete_bucket creds bucket in
  let exit_code = 
    match result with
    | `Ok -> print_endline "ok"; 0
    | `Error msg -> print_endline msg; 1
  in
  return exit_code

let list_buckets creds () =
  lwt result = S3.list_buckets creds in
  let exit_code = 
    match result with
      | `Ok bucket_infos -> 
	List.iter (
	  fun b -> 
	    printf "%s\t%s\n" b#creation_date b#name
	) bucket_infos;
	0
      | `Error body -> print_endline body; 1
  in
  return exit_code

let get_object_s creds s3_bucket s3_object () =
  lwt result = S3.get_object_s (Some creds) ~s3_bucket ~s3_object in
  let exit_code = 
    match result with
      | `Ok body -> print_string body; 0
      | `NotFound -> printf "%s/%s not found\n%!" s3_bucket s3_object; 0
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let get_object creds s3_bucket s3_object path () =
  lwt result = S3.get_object (Some creds) ~s3_bucket ~s3_object ~path in
  let exit_code = 
    match result with
      | `Ok -> print_endline "ok"; 0
      | `NotFound -> printf "%s/%s not found\n%!" s3_bucket s3_object; 0
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let put_object creds s3_bucket s3_object path () =
  lwt result = S3.put_object creds ~s3_bucket ~s3_object ~body:(`File path) in
  let exit_code = 
    match result with
      | `Ok -> 0
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let put_object_s creds s3_bucket s3_object contents () =
  lwt result = S3.put_object creds ~s3_bucket ~s3_object ~body:(`String contents) in
  let exit_code = 
    match result with
      | `Ok -> 0
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let print_kv_list kv_list =
  List.iter (
    fun (k,v) ->
      printf "%s: %s\n" k v
  ) kv_list

let get_object_metadata creds s3_bucket s3_object () =
  lwt result = S3.get_object_metadata creds ~s3_bucket ~s3_object in
  let exit_code = 
    match result with
      | `Ok m -> 
	print_kv_list [
	  "Content-Type", m#content_type;
	  "Content-Length", string_of_int m#content_length;
	  "ETag", m#etag;
	  "Last-Modified", m#last_modified
	];
	0
      | `NotFound -> printf "%S/%S not found\n%!" s3_bucket s3_object; 1
      | `Error msg -> print_endline msg; 1
  in
  return exit_code

let some_or_empty = function
  | Some s -> s
  | None -> ""

let list_objects creds s3_bucket () =
  lwt result = S3.list_objects creds ~s3_bucket in
  let exit_code = 
    match result with
      | `Ok res -> 
	print_kv_list [ 
	  "name", res#name; 
	  "prefix", (some_or_empty res#prefix);
	  "marker", (some_or_empty res#marker);
	  "truncated", (string_of_bool res#is_truncated);
	  "objects", ""
	];
	List.iter (
	  fun o ->
	    printf "%s\t%s\t%s\t%d\t%s\t%s\t%s\n" 
	      o#name 
	      o#last_modified 
	      o#etag 
	      o#size 
	      o#storage_class 
	      o#owner_id 
	      o#owner_display_name
	) res#objects;
	0
      | `NotFound -> printf "bucket %S not found\n%!" s3_bucket; 1
      | `Error msg -> print_endline msg; 1
  in
  return exit_code  

let get_bucket_acl creds s3_bucket () =
  lwt result = S3.get_bucket_acl creds ~s3_bucket in
  let exit_code = 
    match result with
      | `Ok r -> 
	print_kv_list [
	  "owner-id", r#owner_id;
	  "owner-display-name", r#owner_display_name;
	  "grantee", (S3.string_of_grantee r#grantee);
	  "permission", (S3.string_of_permission r#permission)
	];
	0
      | `NotFound -> Printf.printf "bucket %S not found\n%!" s3_bucket; 1
      | `Error msg -> print_endline msg; 1
  in
  return exit_code  


let _ = 
  let getenv_else_exit k = 
    try 
      Unix.getenv k
    with Not_found ->
      Printf.printf "environment variable %S not set\n%!" k;
      exit 1
  in

  let creds = {
    aws_access_key_id = getenv_else_exit "AWS_ACCESS_KEY_ID";
    aws_secret_access_key = getenv_else_exit "AWS_SECRET_ACCESS_KEY"
  }
  in

  let command = 
    match Sys.argv with
      | [| _; "delete_bucket"; bucket |] -> 
	delete_bucket creds bucket

      | [| _; "create_bucket"; bucket |] -> 
	create_bucket creds bucket 

      | [| _; "get_object_s"; bucket; objekt |] -> 
	get_object_s creds bucket objekt

      | [| _; "get_object"; bucket; objekt; path|] -> 
	get_object creds bucket objekt path

      | [| _; "put_object"; bucket; objekt ; path |] -> 
	put_object creds bucket objekt path

      | [| _; "put_object_s"; bucket; objekt ; contents |] -> 
	put_object_s creds bucket objekt contents

      | [| _; "get_object_metadata"; bucket ; objekt |] -> 
	get_object_metadata creds bucket objekt

      | [| _; "list_objects"; bucket |] -> 
	list_objects creds bucket

      | [| _; "get_bucket_acl"; bucket |] -> 
	get_bucket_acl creds bucket

      | [| _; "list_buckets" |] -> 
	list_buckets creds

      | _ -> 
	print_endline "unknown command" ; exit 1

  in
  let exit_code = Lwt_unix.run (command ()) in
  exit exit_code
