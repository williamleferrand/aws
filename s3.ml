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

module C = CalendarLib.Calendar
module P = CalendarLib.Printer.CalendarPrinter
module K = Cryptokit
module HC = Cohttp.Http_client
module X = Xml

open Lwt

let sprintf = Printf.sprintf

let service_url = "http://s3.amazonaws.com/"

let now_as_string () =
  P.sprint "%a, %d %b %Y %H:%M:%S GMT" (C.now ())

(*
let parse_date_string str =
  P.from_fstring "%Y-%M-%dT%H:%M:%S.%z" str
*)

let now_header () =
  "Date", now_as_string ()

type http_method = [`GET | `POST | `HEAD | `DELETE ]

let string_of_http_method = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"

let sign key string_to_sign =
  let hmac_sha1 = K.MAC.hmac_sha1 key in
  let hashed_string_to_sign = K.hash_string hmac_sha1 string_to_sign in
  Util.base64 hashed_string_to_sign

let authorization_header creds string_to_sign =
  let signature = sign creds.Creds.aws_secret_access_key string_to_sign in
  "Authorization", sprintf "AWS %s:%s" creds.Creds.aws_access_key_id signature

exception Error of string

let error_msg body = 
  (* <Error><Code>SomeMessage</Code>...</Error> *)
  match X.parse_string body with
    | X.Element ("Error",_, (X.Element ("Code",_, [X.PCData msg])) :: _ ) -> 
      return (`Error msg)
    | _ -> 
      (* complain if can't interpret the xml, and then just use the
	 entire body as the payload for the exception *)
      fail (Error body)

let get_object_h creds_opt ~s3_bucket ~s3_object =
  let now = now_as_string () in
  let authorization_header = 
    match creds_opt with
      | None -> [] (* anonymous *)
      | Some creds ->
	let string_to_sign = sprintf "%s\n\n\n%s\n/%s/%s" 
	  (string_of_http_method `GET) now s3_bucket s3_object
	in
	[ authorization_header creds string_to_sign ]
  in
	
  let headers = ("Date", now) :: authorization_header in
  let request_url = sprintf "%s%s/%s" service_url 
    (Util.encode_url s3_bucket) (Util.encode_url s3_object) 
  in
  headers, request_url

let get_object_s creds_opt ~s3_bucket ~s3_object =
  let headers, request_url = get_object_h creds_opt ~s3_bucket ~s3_object in
  try_lwt
    lwt _, body = HC.get ~headers:headers request_url in
    return (`Ok body)
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_, _, body) -> error_msg body

let get_object creds_opt ~s3_bucket ~s3_object ~path =
  let headers, request_url = get_object_h creds_opt ~s3_bucket ~s3_object in
  let flags = [ Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND ] in
  let outchan = Lwt_io.open_file ~flags ~mode:Lwt_io.output path in
  lwt res = 
    try_lwt
      lwt _ = HC.get_to_chan ~headers:headers request_url outchan in
      return `Ok
    with 
      | HC.Http_error (404,_,_) -> return `NotFound
      | HC.Http_error (_, _, body) -> error_msg body
  in
  lwt () = Lwt_io.close outchan in
  return res
  

type acl = [`Private]

let string_of_acl = function
  | `Private -> "private"

let create_bucket creds s3_bucket acl =
  let now = now_as_string () in
  let acl_s = string_of_acl acl in
  let string_to_sign = sprintf "%s\n\n\n%s\nx-amz-acl:%s\n/%s"
    (string_of_http_method `PUT) now acl_s s3_bucket 
  in
  let authorization_header = authorization_header creds string_to_sign in
  let request_url = sprintf "http://s3.amazonaws.com/%s" s3_bucket in
  let headers = [
    authorization_header;
    "Date", now; 
    "x-amz-acl", acl_s
  ]
  in
  try_lwt
    lwt _ = HC.put ~headers:headers ?body:None request_url in
    return `Ok
  with HC.Http_error (_, _, body) ->
    error_msg body


let delete_bucket creds s3_bucket =
  let now = now_as_string () in
  let string_to_sign = sprintf "%s\n\n\n%s\n/%s"
    (string_of_http_method `DELETE) now s3_bucket 
  in
  let authorization_header = authorization_header creds string_to_sign in
  let request_url = sprintf "http://s3.amazonaws.com/%s" s3_bucket in
  let headers = [ authorization_header; "Date", now ] in
  try_lwt
    lwt _ = HC.delete ~headers:headers request_url in
    (* an `Ok response actually transmitted via a 204 *)
    fail (Error "delete_bucket")
  with 
    | HC.Http_error (204, _, _) ->
      return `Ok
    | HC.Http_error (_,_,body) ->
      error_msg body

let rec bucket = function
  | X.Element (
    "Bucket",_, [
      X.Element ("Name",_,        [X.PCData name           ]) ;  
      X.Element ("CreationDate",_,[X.PCData creation_date_s]) 
    ] )->
    (object
      method name = name
      method creation_date = creation_date_s
     end)
  | _ -> raise (Error "ListAllMyBucketsResult:2")

and list_all_my_buckets_result_of_xml = function 
  | X.Element ("ListAllMyBucketsResult",_, [_ ; X.Element ("Buckets",_,buckets) ] ) ->
    List.map bucket buckets
  | _ -> raise (Error "ListAllMyBucketsResult:1")

let list_buckets creds =
  let now = now_as_string () in
  let string_to_sign = sprintf "%s\n\n\n%s\n/" (string_of_http_method `GET) now in
  let authorization_header = authorization_header creds string_to_sign in
  let headers = [ authorization_header ; "Date", now ] in
  let request_url = service_url in
  try_lwt
    lwt headers, body = HC.get ~headers:headers request_url in
    try
      let buckets = list_all_my_buckets_result_of_xml (X.parse_string body) in
      return (`Ok buckets)
    with (Error _) as exn ->
      fail exn
  with HC.Http_error (_, _, body) ->
    error_msg body


let noop () = return ()

let put_object 
    ?(content_type="binary/octet-stream")
    ?(acl=`Private)
    creds 
    ~s3_bucket 
    ~s3_object 
    ~body =
  let now = now_as_string () in
  let bucket_object = (Util.encode_url s3_bucket) ^ "/" ^ 
    (Util.encode_url s3_object) 
  in
  let request_url = service_url ^ bucket_object in
  let acl_s = string_of_acl acl in
  let headers = [
    "Date", now;
    "x-amz-acl", acl_s;
    "Content-Type", content_type
  ]
  in
  let string_to_sign = sprintf "%s\n\n%s\n%s\nx-amz-acl:%s\n/%s" 
    (string_of_http_method `PUT) content_type now acl_s bucket_object in
  let authorization_header = authorization_header creds string_to_sign in
  let headers = authorization_header :: headers in
  (* [close] closes the input channel, if any *)
  let request_body, close =
    match body with
      | `String contents -> 
	`String contents, noop
      | `File path -> 
	let file_size = Util.file_size path in
	let flags = [Unix.O_RDONLY] in
	let inchan = Lwt_io.open_file ~flags ~mode:Lwt_io.input path in
	`InChannel (file_size, inchan), fun _ -> Lwt_io.close inchan
  in
  try_lwt
    lwt _ = HC.put ~headers:headers ~body:request_body request_url in
    lwt () = close () in
    return `Ok
  with 
    | HC.Http_error (_, _, body) ->
      lwt () = close () in
      error_msg body
    | exn ->
      lwt () = close () in
      raise exn


let assoc_header headers err name =
  let name = String.lowercase name in
  try
    let _, v = List.find (fun (n,v) -> (String.lowercase n) = name) headers in
    v
  with Not_found ->
    raise (Error err)

let get_object_metadata creds ~s3_bucket ~s3_object =
  let now = now_as_string () in
  let string_to_sign = sprintf "%s\n\n\n%s\n/%s/%s" 
    (string_of_http_method `HEAD) now s3_bucket s3_object in
  let authorization_header = authorization_header creds string_to_sign in
  let headers = [ "Date", now ; authorization_header ] in
  let bucket_object = (Util.encode_url s3_bucket) ^ "/" ^ 
    (Util.encode_url s3_object) in
  let request_url = service_url ^ bucket_object in
  try_lwt
    lwt response_headers, _ = HC.head ~headers request_url in
    let find k = assoc_header response_headers ("GetObjectMetadata:" ^ k) k in
    let content_type = find "Content-Type" in
    let etag = find "ETag" in
    let last_modified = find "Last-Modified" in
    let content_length = int_of_string (find "Content-Length") in
    let meta = (object 
      method content_type = content_type
      method etag = etag
      method last_modified = last_modified
      method content_length = content_length
    end)
    in
    return (`Ok meta)
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body
    
  
let option_pcdata err = function
  | [X.PCData x] -> Some x
  | [] -> None
  | _ -> raise (Error err)

let rec list_bucket_result_of_xml = function
  | X.Element ("ListBucketResult",_,kids) -> (
    match kids with 
      | X.Element ("Name",_,[X.PCData name]) ::
	  X.Element ("Prefix",_,prefix_opt) ::
	  X.Element ("Marker",_,marker_opt) ::
	  X.Element ("MaxKeys",_,[X.PCData max_keys]) ::
	  X.Element ("IsTruncated",_,[X.PCData is_truncated]) ::
	  contents ->

	let prefix_opt = option_pcdata "ListBucketResult:prefix" prefix_opt in
	let marker_opt = option_pcdata "ListBucketResult:marker" marker_opt in
	let max_keys = int_of_string max_keys in
	let is_truncated = bool_of_string is_truncated in
	let contents = contents_of_xml contents in

        (object 
	  method name = name
	  method prefix = prefix_opt
	  method marker = marker_opt
	  method max_keys = max_keys
	  method is_truncated = is_truncated
	  method objects = contents
	 end)
      | _ ->
	raise (Error "ListBucketResult:k")
  )
  | _ ->
    raise (Error "ListBucketResult:t")

and contents_of_xml contents =
  List.map objects_of_xml contents

and objects_of_xml = function
  | X.Element ("Contents",_, [
    X.Element ("Key",_,[X.PCData name]);
    X.Element ("LastModified",_,[X.PCData last_modified]);
    X.Element ("ETag",_,[X.PCData etag]);
    X.Element ("Size",_,[X.PCData size]);
    X.Element ("Owner",_,[
      X.Element ("ID",_,[X.PCData owner_id]);
      X.Element ("DisplayName",_,[X.PCData owner_display_name])
    ]);
    X.Element ("StorageClass",_,[X.PCData storage_class])
  ]) ->
    let size = int_of_string size in
    (object 
      method name = name
      method last_modified = last_modified (* TODO Calendar.t *)
      method etag = etag
      method size = size
      method storage_class = storage_class 
      method owner_id = owner_id
      method owner_display_name = owner_display_name
     end)
  | _ -> raise (Error "ListBucketResult:c")
    

let list_objects creds s3_bucket =
  let now = now_as_string () in
  let string_to_sign = sprintf "%s\n\n\n%s\n/%s"
    (string_of_http_method `GET) now s3_bucket in
  let authorization_header = authorization_header creds string_to_sign in
  let headers = [ "Date", now ; authorization_header ] in
  let request_url = service_url ^ (Util.encode_url s3_bucket) in
  try_lwt
    lwt response_headers, response_body = HC.get ~headers request_url in
    return (`Ok (list_bucket_result_of_xml (X.parse_string response_body)))
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body


type permission = [
| `read 
| `write
| `read_acp
| `write_acp
| `full_control
]

let string_of_permission = function
  | `read -> "READ"
  | `write -> "WRITE"
  | `read_acp -> "READ_ACP"
  | `write_acp -> "WRITE_ACP"
  | `full_control -> "FULL_CONTROL"

let permission_of_string = function
  | "READ" -> `read 
  | "WRITE" -> `write
  | "READ_ACP" -> `read_acp
  | "WRITE_ACP" -> `write_acp
  | "FULL_CONTROL" -> `full_control
  | x -> raise (Error (sprintf "invalid permission %S" x))


type grantee = [ 
| `amazon_customer_by_email of string
| `canonical_user of < display_name : string; id : string >
| `group of string 
]

let string_of_grantee = function
| `amazon_customer_by_email em -> "AmazonCustomerByEmail " ^ em
| `canonical_user cn -> sprintf "CanonicalUser (%s,%s)" cn#id cn#display_name
| `group g -> "Group " ^ g


let grantee_of_xml = function 
  | [X.Element ("ID",_,[X.PCData id]);
     X.Element ("DisplayName",_,[X.PCData display_name])
    ] ->
    `canonical_user (object 
      method id = id 
      method display_name = display_name
    end)

  | [X.Element ("EmailAddress",_,[X.PCData email_address])] ->
    `amazon_customer_by_email email_address

  | [X.Element ("URI",_,[X.PCData group])] ->
    `group group

  | _ ->
    raise (Error "grantee")

let access_control_policy_of_xml = function
  | X.Element ("AccessControlPolicy",_,[
    X.Element ("Owner",_,[
      X.Element ("ID",_,[X.PCData owner_id]);
      X.Element ("DisplayName",_,[X.PCData owner_display_name])
    ]);
    X.Element ("AccessControlList",_,[
      X.Element ("Grant",_,[
	X.Element ("Grantee", grantee_atts, grantee_x);
	X.Element ("Permission",_,[X.PCData permission_s])
      ])
    ])
  ]) ->
    let grantee = grantee_of_xml grantee_x in
    let permission = permission_of_string permission_s in

    (object
      method owner_id = owner_id
      method owner_display_name = owner_display_name
      method grantee = grantee
      method permission = permission
     end)
  | _ ->
    raise (Error "AccessControlPolicy:t")

let get_bucket_acl creds s3_bucket =
  let now = now_as_string () in
  let string_to_sign = sprintf "%s\n\n\n%s\n/%s/?acl" 
      (string_of_http_method `GET) now s3_bucket 
  in
  let authorization_header = authorization_header creds string_to_sign in  
  let headers = [ "Date", now ; authorization_header ] in
  let request_url = service_url ^ (Util.encode_url s3_bucket) ^ "/?acl" in
  try_lwt
    lwt response_headers, response_body = HC.get ~headers request_url in
    return (`Ok (access_control_policy_of_xml (X.parse_string response_body))) 
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body

  

  
