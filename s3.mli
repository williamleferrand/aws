(** client module to Amazon's S3 service *)

exception Error of string

type region = [ 
| `US_EAST_1 
| `US_WEST_1 
| `EU_WEST_1 
| `AP_SOUTHEAST_1 
| `AP_NORTHEAST_1
]

val string_of_region : region -> string
val region_of_string : string -> region

type amz_acl = [ 
| `Private 
| `public_read 
| `public_read_write 
| `authenticated_read 
| `bucket_owner_read 
| `bucket_owner_full_control 
]

(* Note: `PermanentRedirect is a result of many of the functions
   below.  If the specified region is us-east-1, and this not the
   correct region for the bucket in question, the target region of is
   unknown. *)

val create_bucket : Creds.t ->  region -> string -> amz_acl -> 
  [> `Error of string | `Ok ] Lwt.t

val delete_bucket : Creds.t ->  region -> string ->  
  [> `Error of string | `Ok | `PermanentRedirect of region option ] Lwt.t

val list_buckets : Creds.t -> region ->
  [> `Error of string 
  | `Ok of < creation_date : string; name : string > list 
  | `PermanentRedirect of region option 
  ] Lwt.t

val get_object_s : 
  Creds.t option ->
  region ->
  bucket:string ->
  objekt:string -> 
  [> `NotFound | `Error of string | `Ok of string | `AccessDenied | `PermanentRedirect of region option ] Lwt.t

val get_object :
  ?byte_range: (int * int) ->
  Creds.t option ->
  region -> 
  bucket:string ->
  objekt:string ->
  path:string ->
  [> `Error of string | `NotFound | `Ok | `AccessDenied | `PermanentRedirect of region option ] Lwt.t

val put_object : 
  ?content_type:string ->
  ?amz_acl:amz_acl ->
  Creds.t ->
  region ->  
  bucket:string ->
  objekt:string -> 
  body:[ `File of string | `String of string ] ->
  [> `Error of string | `Ok | `AccessDenied | `PermanentRedirect of region option ] Lwt.t

val get_object_metadata :
  Creds.t ->
  region ->
  bucket:string ->
  objekt:string -> 
  [> `NotFound 
  | `Error of string 
  | `Ok of < 
      content_length : int; 
   content_type : string; 
   etag : string; 
   last_modified : float
   > 
  | `PermanentRedirect of region option
  ] Lwt.t

    
val list_objects :
  Creds.t ->
  region ->
  string -> 
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
   last_modified : float;
   name : string; 
   owner_display_name : string;
   owner_id : string; 
   size : int;
   storage_class : string 
   > list;
   > 
  | `PermanentRedirect of region option
  ] Lwt.t

type permission = [
| `read 
| `write
| `read_acp
| `write_acp
| `full_control
]
val string_of_permission : permission -> string
val permission_of_string : string -> permission

type identity = [ 
| `amazon_customer_by_email of string
| `canonical_user of < display_name : string; id : string >
| `group of string 
]
val string_of_identity : identity -> string

class canonical_user : id:string -> display_name:string -> 
object 
  method display_name : string 
  method id : string 
end

type grant = identity * permission

class acl : identity -> grant list ->
object
  method grants : grant list
  method owner : identity
end

val get_bucket_acl :
  Creds.t ->
  region ->
  string -> 
  [> `Error of string | `NotFound | `Ok of acl | `PermanentRedirect of region option ] Lwt.t

val set_bucket_acl :
  Creds.t ->
  region ->
  string ->
  acl ->
  [> `Error of string | `NotFound | `Ok | `PermanentRedirect of region option ] Lwt.t


val delete_object :
  Creds.t ->
  region ->
  bucket:string ->
  objekt:string ->
  [> `Error of string | `BucketNotFound | `Ok | `PermanentRedirect of region option ] Lwt.t

val get_object_acl :
  Creds.t ->
  region ->
  bucket:string ->
  objekt:string ->
  [> `Error of string | `NotFound | `Ok of acl | `PermanentRedirect of region option ] Lwt.t  

val set_object_acl :
  Creds.t ->
  region ->
  bucket:string ->
  objekt:string ->
  acl ->
  [> `Error of string | `NotFound | `Ok | `PermanentRedirect of region option ] Lwt.t  


val get_bucket_policy :
  Creds.t ->
  region ->
  bucket:string ->
  [> `AccessDenied | `Error of string | `NotFound | `NotOwner | `Ok of string ] Lwt.t

val delete_bucket_policy :
  Creds.t ->
  region ->
  bucket:string ->
  [> `AccessDenied | `Error of string | `NotOwner | `Ok ] Lwt.t

val set_bucket_policy :
  Creds.t ->
  region ->
  bucket:string ->
  policy:string ->
  [> `AccessDenied | `Error of string | `Ok | `MalformedPolicy ] Lwt.t

(* Copyright (c) 2011, barko 00336ea19fcb53de187740c490f764f4 All
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

