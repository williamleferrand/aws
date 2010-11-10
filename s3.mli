(** client module to Amazon's S3 service *)

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

exception Error of string

type acl = [ `Private ]

val create_bucket : Creds.t ->  string ->  acl -> 
  [> `Error of string | `Ok ] Lwt.t

val delete_bucket : Creds.t ->  string ->  
  [> `Error of string | `Ok ] Lwt.t

val list_buckets : Creds.t ->   
  [> `Error of string 
  | `Ok of < creation_date : string; name : string > list ] Lwt.t

val get_object_s : 
  Creds.t option ->
  s3_bucket:string ->
  s3_object:string -> 
  [> `NotFound | `Error of string | `Ok of string ] Lwt.t

val get_object :
  Creds.t option ->
  s3_bucket:string ->
  s3_object:string ->
  path:string ->
  [> `Error of string | `NotFound | `Ok ] Lwt.t

val put_object : 
  ?content_type:string ->
  ?acl:acl ->
  Creds.t ->  
  s3_bucket:string ->
  s3_object:string -> 
  body:[ `File of string | `String of string ] ->
  [> `Error of string | `Ok ] Lwt.t

val get_object_metadata :
  Creds.t ->
  s3_bucket:string ->
  s3_object:string -> 
  [> `NotFound 
  | `Error of string 
  | `Ok of < 
      content_length : int; 
   content_type : string; 
   etag : string; 
   last_modified : string (* TODO : CalendarLib.Calendar.t *)
    > 
  ] Lwt.t

  
val list_objects :
  Creds.t ->
  s3_bucket:string -> 
  [> `Error of string 
  | `NotFound 
  | `Ok of < 
      name : string; 
      prefix : string option;
      marker : string option; 
      max_keys : int; 
      is_truncated : bool; 
      objects : < 
	etag : string; 
        last_modified : string;
        name : string; 
	owner_display_name : string;
        owner_id : string; 
	size : int;
        storage_class : string 
      > list;
    > 
  ] Lwt.t

type permission = [
| `read 
| `write
| `read_acp
| `write_acp
| `full_control
]
val string_of_permission : permission -> string

type grantee = [ 
| `amazon_customer_by_email of string
| `canonical_user of < display_name : string; id : string >
| `group of string 
]
val string_of_grantee : grantee -> string

val get_bucket_acl :
  Creds.t ->
  s3_bucket:string -> 
  [> `Error of string 
  | `NotFound 
  | `Ok of < 
      grantee : grantee;
      owner_display_name : string; 
      owner_id : string;
      permission : permission
    >
  ] Lwt.t
