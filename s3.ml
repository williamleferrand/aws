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

open Http_method
open Lwt

let sprintf = Printf.sprintf

let service_url = "http://s3.amazonaws.com/"

let now_as_string () =
  P.sprint "%a, %d %b %Y %H:%M:%S GMT" (C.now ())

type amz_acl = [ 
| `Private (* not using [`private] because [private] is an ocaml keyword *)
| `public_read 
| `public_read_write 
| `auauthenticated_read 
| `buckbucket_owner_read 
| `bucket_owner_full_control 
]


let string_of_amz_acl = function
  | `Private -> "private"
  | `public_read -> "public-read"
  | `public_read_write -> "public-read-write"
  | `auauthenticated_read -> "authenticated-read"
  | `buckbucket_owner_read -> "bucket-owner-read"
  | `bucket_owner_full_control -> "bucket-owner-full-control"

let sign key string_to_sign =
  let hmac_sha1 = K.MAC.hmac_sha1 key in
  let hashed_string_to_sign = K.hash_string hmac_sha1 string_to_sign in
  Util.base64 hashed_string_to_sign

exception Error of string

module StringMap = Map.Make(String)

let group_by_key kv_list =
  let map = List.fold_left (
    fun map (k,v) ->
      let v_list =
        try 
          StringMap.find k map
        with Not_found ->
          []
      in
      StringMap.add k (v :: v_list) map
  ) StringMap.empty kv_list
  in
  StringMap.fold (
    fun k v_list accu ->
      (k, v_list) :: accu
  ) map []

(* replace a substring of whitespace with a single space *)
let squash_whitespace = 
  Pcre.replace ~rex:(Pcre.regexp "[[:space:]]+") ~templ:" "

type sub_resource = [ 
| `acl 
| `location
| `logging
| `notification
| `part_number
| `policy
| `request_payment
| `torrent
| `upload_id
| `uploads
| `version_id
| `versioning
| `versions
]

let sub_resource_of_string = function
| "acl"            -> `acl 
| "location"       -> `location
| "logging"        -> `logging
| "notification"   -> `notification
| "partNumber"     -> `part_number
| "policy"         -> `policy
| "requestPayment" -> `request_payment
| "torrent"        -> `torrent 
| "uploadId"       -> `upload_id
| "uploads"        -> `uploads
| "versionId"      -> `version_id
| "versioning"     -> `versioning
| "versions"       -> `versions
| _ -> raise (Error "subresource")

let string_of_sub_resource = function
| `acl              -> "acl"            
| `location         -> "location"       
| `logging          -> "logging"        
| `notification     -> "notification"   
| `part_number      -> "partNumber"     
| `policy           -> "policy"         
| `request_payment  -> "requestPayment" 
| `torrent          -> "torrent"        
| `upload_id        -> "uploadId"       
| `uploads          -> "uploads"        
| `version_id       -> "versionId"      
| `versioning       -> "versioning"     
| `versions         -> "versions"       

class buffer size =
  let b = Buffer.create size in
object
  method add s = Buffer.add_string b s
  method contents = Buffer.contents b
end

let auth_hdr
    ?(http_method=`GET)
    ?(content_type="")
    ?(content_md5="")
    ?(date="")
    ?(bucket="") 
    ?(request_uri="")
    ?(amz_headers=[])
    ?(sub_resources=[])
    creds =

  let canonicalized_amz_headers = 
    match amz_headers with
      | [] -> ""
      | _ -> (
        let downcased_keys = List.map (
          fun (k,v) -> String.lowercase k, squash_whitespace v
        ) amz_headers 
        in
        let grouped = group_by_key downcased_keys in
        let merged_values = List.map (
          fun (k,v_list) -> k, (String.concat "," v_list)
        ) grouped 
        in
        let sorted_by_key = Util.sort_assoc_list merged_values in

        let buf = new buffer 10 in
        List.iter (
          fun (k,v) ->
            buf#add k; buf#add ":"; buf#add v; buf#add "\n"
        ) sorted_by_key;
        buf#contents
      )
  in

  let canonicalized_sub_resources = 
    match sub_resources with
      | [] -> ""
      | _ -> (
        let sub_resources_s = List.map (
          fun (k,vo) -> string_of_sub_resource k, vo
        ) sub_resources
        in
        let sorted_sub_resources_s = Util.sort_assoc_list sub_resources_s in

        let buf = new buffer 10 in
        buf#add "?";

        List.iter (
          fun (k, v_opt) ->
            match v_opt with
              | Some v -> buf#add k; buf#add "&"; buf#add v
              | None -> buf#add k
        ) sorted_sub_resources_s;
        buf#contents
      )
  in
  let canonicalized_resource =
    "/" ^ bucket ^ request_uri ^ canonicalized_sub_resources
  in

  let string_to_sign = 
    let buf = new buffer 100 in
    buf#add (string_of_http_method http_method); buf#add "\n";
    buf#add content_md5; buf#add "\n";
    buf#add content_type; buf#add "\n";
    buf#add date; buf#add "\n";
    buf#add canonicalized_amz_headers;
    buf#add canonicalized_resource;
    buf#contents
  in
  let signature = sign creds.Creds.aws_secret_access_key string_to_sign in
  "Authorization", sprintf "AWS %s:%s" creds.Creds.aws_access_key_id signature

let error_msg body = 
  (* <Error><Code>SomeMessage</Code>...</Error> *)
  match X.parse_string body with
    | X.E ("Error",_, (X.E ("Code",_, [X.P msg])) :: _ ) -> 
      return (`Error msg)
    | _ -> 
        (* complain if can't interpret the xml, and then just use the
           entire body as the payload for the exception *)
      fail (Error body)

let get_object_h creds_opt ~bucket ~objekt =
  let date = now_as_string () in
  let authorization_header = 
    match creds_opt with
      | None -> [] (* anonymous *)
      | Some creds ->
        [ auth_hdr 
          ~http_method:`GET 
          ~date 
          ~bucket 
          ~request_uri:("/" ^ objekt) 
          creds
        ]
  in
        
  let headers = ("Date", date) :: authorization_header in
  let request_url = sprintf "%s%s/%s" service_url 
    (Util.encode_url bucket) (Util.encode_url objekt) 
  in
  headers, request_url

(* get object *)
let get_object_s creds_opt ~bucket ~objekt =
  let headers, request_url = get_object_h creds_opt ~bucket ~objekt in
  try_lwt
    lwt _, body = HC.get ~headers request_url in
    return (`Ok body)
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_, _, body) -> error_msg body

let get_object ?byte_range creds_opt ~bucket ~objekt ~path =
  let headers, request_url = get_object_h creds_opt ~bucket ~objekt in
  let byte_range_header = 
    match byte_range with
      | None -> []
      | Some (start,fini) -> ["Range", sprintf "bytes=%d-%d" start fini]
  in
  let headers = headers @ byte_range_header in
  let flags = [ Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND; Unix.O_TRUNC ] in
  let outchan = Lwt_io.open_file ~flags ~mode:Lwt_io.output path in
  lwt res = 
    try_lwt
      lwt _ = HC.get_to_chan ~headers request_url outchan in
      return `Ok
    with 
      | HC.Http_error (404,_,_) -> return `NotFound
      | HC.Http_error (_, _, body) -> error_msg body
  in
  lwt () = Lwt_io.close outchan in
  return res
  
(* create bucket *)
let create_bucket creds bucket amz_acl =
  let date = now_as_string () in
  let amz_headers = ["x-amz-acl", string_of_amz_acl amz_acl ] in
  let authorization_header = auth_hdr 
    ~http_method:`PUT ~bucket ~amz_headers ~date creds  
  in
  let request_url = sprintf "%s%s" service_url bucket in
  let headers = authorization_header :: ("Date", date) :: amz_headers in
  try_lwt
    lwt _ = HC.put ~headers request_url in
    return `Ok
  with HC.Http_error (_, _, body) ->
    error_msg body

(* delete bucket *)
let delete_bucket creds bucket =
  let date = now_as_string () in
  let authorization_header = auth_hdr 
    ~http_method:`DELETE ~bucket ~date creds  
  in
  let request_url = sprintf "%s%s" service_url (Util.encode_url bucket) in
  let headers = [ authorization_header ; "Date", date ] in
  try_lwt
    lwt _ = HC.delete ~headers request_url in
    (* success signaled via a 204 *)
    fail (Error "delete_bucket")
  with 
    | HC.Http_error (204, _, _) ->
      return `Ok
    | HC.Http_error (_,_,body) ->
      error_msg body

(* list buckets *)
let rec bucket = function
  | X.E (
    "Bucket",_, [
      X.E ("Name",_,        [X.P name           ]) ;  
      X.E ("CreationDate",_,[X.P creation_date_s]) 
    ] )->
    (object
      method name = name
      method creation_date = creation_date_s
     end)
  | _ -> raise (Error "ListAllMyBucketsResult:2")

and list_all_my_buckets_result_of_xml = function 
  | X.E ("ListAllMyBucketsResult",_, [_ ; X.E ("Buckets",_,buckets) ] ) ->
    List.map bucket buckets
  | _ -> raise (Error "ListAllMyBucketsResult:1")

let list_buckets creds =
  let date = now_as_string () in
  let authorization_header = auth_hdr ~http_method:`GET ~date creds in
  let headers = [ authorization_header ; "Date", date ] in
  let request_url = service_url in
  try_lwt
    lwt headers, body = HC.get ~headers request_url in
    try
      let buckets = list_all_my_buckets_result_of_xml (X.parse_string body) in
      return (`Ok buckets)
    with (Error _) as exn ->
      fail exn
  with HC.Http_error (_, _, body) ->
    error_msg body


(* put object *)
let noop () = return ()

let put_object 
    ?(content_type="binary/octet-stream")
    ?(amz_acl=`Private)
    creds 
    ~bucket 
    ~objekt 
    ~body =
  let date = now_as_string () in
  let amz_headers = [ "x-amz-acl", string_of_amz_acl amz_acl ] in
  let authorization_header = auth_hdr 
    ~http_method:`PUT 
    ~content_type 
    ~amz_headers
    ~date 
    ~bucket 
    ~request_uri:("/" ^ objekt) 
    creds
  in
  let bucket_object = (Util.encode_url bucket) ^ "/" ^ 
    (Util.encode_url objekt) 
  in
  let request_url = service_url ^ bucket_object in
  let headers = ("Date", date) :: ("Content-Type", content_type) ::
    authorization_header :: amz_headers
  in
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
    lwt _ = HC.put ~headers ~body:request_body request_url in
    lwt () = close () in
    return `Ok
  with 
    | HC.Http_error (_, _, body) ->
      lwt () = close () in
      error_msg body
    | exn ->
      lwt () = close () in
      raise exn


(* get object metadata *)
let assoc_header headers err name =
  let name = String.lowercase name in
  try
    let _, v = List.find (fun (n,v) -> (String.lowercase n) = name) headers in
    v
  with Not_found ->
    raise (Error err)

let get_object_metadata creds ~bucket ~objekt =
  let date = now_as_string () in
  let authorization_header = auth_hdr
    ~http_method:`HEAD ~date ~bucket ~request_uri:("/" ^ objekt) creds
  in
  let headers = [ "Date", date ; authorization_header ] in
  let bucket_object = (Util.encode_url bucket) ^ "/" ^ (Util.encode_url objekt) in
  let request_url = service_url ^ bucket_object in
  try_lwt
    lwt response_headers, _ = HC.head ~headers request_url in
    let find k = assoc_header response_headers ("GetObjectMetadata:" ^ k) k in
    let content_type = find "Content-Type" in
    let etag = find "ETag" in
    let last_modified_s = find "Last-Modified" in
    let content_length = int_of_string (find "Content-Length") in
    let last_modified = Util.parse_amz_date_string last_modified_s in
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
    
  
(* list objects *)
let option_pcdata err = function
  | [X.P x] -> Some x
  | [] -> None
  | _ -> raise (Error err)

let rec list_bucket_result_of_xml = function
  | X.E ("ListBucketResult",_,kids) -> (
    match kids with 
      | X.E ("Name",_,[X.P name]) ::
          X.E ("Prefix",_,prefix_opt) ::
          X.E ("Marker",_,marker_opt) ::
          X.E ("MaxKeys",_,[X.P max_keys]) ::
          X.E ("IsTruncated",_,[X.P is_truncated]) ::
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
  | X.E ("Contents",_, [
    X.E ("Key",_,[X.P name]);
    X.E ("LastModified",_,[X.P last_modified_s]);
    X.E ("ETag",_,[X.P etag]);
    X.E ("Size",_,[X.P size]);
    X.E ("Owner",_,[
      X.E ("ID",_,[X.P owner_id]);
      X.E ("DisplayName",_,[X.P owner_display_name])
    ]);
    X.E ("StorageClass",_,[X.P storage_class])
  ]) ->
    let last_modified = Util.parse_amz_date_string last_modified_s in
    let size = int_of_string size in
    (object 
      method name = name
      method last_modified = last_modified
      method etag = etag
      method size = size
      method storage_class = storage_class 
      method owner_id = owner_id
      method owner_display_name = owner_display_name
     end)
  | _ -> raise (Error "ListBucketResult:c")
    

let list_objects creds bucket =
  let date = now_as_string () in
  let authorization_header = auth_hdr ~http_method:`GET ~date ~bucket creds in
  let headers = [ "Date", date ; authorization_header ] in
  let request_url = service_url ^ (Util.encode_url bucket) in
  try_lwt
    lwt response_headers, response_body = HC.get ~headers request_url in
    return (`Ok (list_bucket_result_of_xml (X.parse_string response_body)))
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body

(* get bucket acl *)
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


class canonical_user ~id ~display_name =
object
  method id : string = id
  method display_name : string = display_name
end

type identity = [ 
| `amazon_customer_by_email of string
| `canonical_user of canonical_user
| `group of string 
]

type grant = identity * permission

class acl owner grants =
object 
  method owner : identity = owner
  method grants : grant list = grants
end


let string_of_identity = function
| `amazon_customer_by_email em -> "AmazonCustomerByEmail " ^ em
| `canonical_user cn -> sprintf "CanonicalUser (%s,%s)" cn#id cn#display_name
| `group g -> "Group " ^ g

let tag_of_identity = function 
| `amazon_customer_by_email _ -> "AmazonCustomerByEmail"
| `canonical_user _ -> "CanonicalUser"
| `group _ -> "Group"

let identity_of_xml = function 
  | [X.E ("ID",_,[X.P id]);
     X.E ("DisplayName",_,[X.P display_name])
    ] ->
    `canonical_user (new canonical_user id display_name)

  | [X.E ("EmailAddress",_,[X.P email_address])] ->
    `amazon_customer_by_email email_address

  | [X.E ("URI",_,[X.P group])] ->
    `group group

  | _ ->
    raise (Error "grantee")

let grant_of_xml = function
  | X.E ("Grant",_, [
    X.E ("Grantee", grantee_atts, grantee_x);
    X.E ("Permission",_,[X.P permission_s])  ]) ->
    let grantee = identity_of_xml grantee_x in
    let permission = permission_of_string permission_s in
    (grantee, permission)

  | _ ->
    raise (Error "Grant")

let access_control_policy_of_xml = function
  | X.E ("AccessControlPolicy",_,[
    X.E ("Owner", _, owner_x);
    X.E ("AccessControlList", _, grants_x) ]) ->
    let owner = identity_of_xml owner_x in
    let grants = List.map grant_of_xml grants_x in
    new acl owner grants
  | _ ->
    raise (Error "AccessControlPolicy:t")

let get_bucket_acl creds bucket =
  let date = now_as_string () in
  let authorization_header = auth_hdr 
    ~http_method:`GET 
    ~date 
    ~bucket 
    ~sub_resources:[`acl, None] 
    creds
  in
  let headers = [ "Date", date ; authorization_header ] in
  let request_url = service_url ^ 
    (Util.encode_url bucket) ^ "?" ^ (string_of_sub_resource `acl) 
  in
  try_lwt
    lwt response_headers, response_body = HC.get ~headers request_url in
    return (`Ok (access_control_policy_of_xml (X.parse_string response_body))) 
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body

  
(* set bucket acl *)
let xml_of_permission permission = 
  X.E ("Permission",[],[X.P (string_of_permission permission)])

let ns_schema_instance = "http://www.w3.org/2001/XMLSchema-instance"
let ns_xmlns = "http://www.w3.org/2000/xmlns/"

let atts s = [ 
  (ns_xmlns          , "xsi" ), ns_schema_instance;
  (ns_schema_instance, "type"), s
]

let xml_of_identity = function
  | `amazon_customer_by_email email ->
    [X.E("EmailAddress",[],[X.P email])]

  | `canonical_user cn ->
    [X.E ("ID",[],[X.P cn#id]);
     X.E ("DisplayName",[],[X.P cn#display_name])]

  | `group uri ->
    [X.E("URI",[],[X.P uri])]

let xml_of_grantee identity = 
  let identity_x = xml_of_identity identity in
  X.E("Grantee", atts (tag_of_identity identity), identity_x)

let xml_of_owner identity =
  let identity_x = xml_of_identity identity in
  X.E ("Owner",[], identity_x)

let xml_of_grant (grantee, permission) = 
  let kids = [xml_of_grantee grantee; xml_of_permission permission] in
  X.E("Grant", [], kids)

let xml_of_access_control_list grants =
  X.E("AccessControlList", [], List.map xml_of_grant grants)

let xml_of_access_control_policy acl =
  let kids = [ 
    xml_of_owner acl#owner ; 
    xml_of_access_control_list acl#grants ] 
  in
  X.E ("AccessControlPolicy", [], kids)

let xml_content_type_header = "Content-Type", "application/xml"

let set_bucket_acl creds bucket acl  =
  let date = now_as_string () in
  let authorization_header = auth_hdr
    ~http_method:`PUT 
    ~content_type:"application/xml"
    ~date 
    ~bucket 
    ~sub_resources:[`acl, None] 
    creds
  in
  let request_url = service_url ^ 
    (Util.encode_url bucket) ^ "?" ^ (string_of_sub_resource `acl) 
  in  
  let headers = [ "Date", date ; xml_content_type_header; authorization_header ] in
  let xml = xml_of_access_control_policy acl in
  let body = `String (X.string_of_xml xml) in
  try_lwt
    lwt _ = HC.put ~headers ~body request_url in
    return `Ok
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body

(* delete object *)
let delete_object creds ~bucket ~objekt =
  let date = now_as_string () in
  let authorization_header = auth_hdr
    ~http_method:`DELETE
    ~date
    ~bucket
    ~request_uri:("/" ^ objekt)
    creds
  in
  let request_url = sprintf "%s%s/%s" service_url (Util.encode_url bucket) 
    (Util.encode_url objekt) 
  in
  let headers = [ "Date", date ; authorization_header ] in    
  try_lwt
    lwt _ = HC.delete ~headers request_url in
    (* success actually signaled via a 204 *)
    fail (Error "delete_object")
  with
    | HC.Http_error (404,_,_) -> return `BucketNotFound
    | HC.Http_error (204,_,_) -> return `Ok
    | HC.Http_error (_,_,body) -> error_msg body

(* get object acl *)
let get_object_acl creds ~bucket ~objekt =
  let date = now_as_string () in
  let authorization_header = auth_hdr 
    ~http_method:`GET 
    ~date 
    ~bucket 
    ~request_uri:("/" ^ objekt)
    ~sub_resources:[`acl, None] 
    creds
  in
  let headers = [ "Date", date ; authorization_header ] in
  let request_url = sprintf "%s%s/%s?%s" service_url 
    (Util.encode_url bucket) (Util.encode_url objekt) 
    (string_of_sub_resource `acl) 
  in
  try_lwt
    lwt response_headers, response_body = HC.get ~headers request_url in
    return (`Ok (access_control_policy_of_xml (X.parse_string response_body))) 
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body

let set_object_acl creds ~bucket ~objekt acl  =
  let date = now_as_string () in
  let authorization_header = auth_hdr
    ~http_method:`PUT 
    ~content_type:"application/xml"
    ~date 
    ~bucket 
    ~request_uri:("/" ^ objekt)
    ~sub_resources:[`acl, None] 
    creds
  in
  let request_url = sprintf "%s%s/%s?%s" service_url  
    (Util.encode_url bucket) (Util.encode_url objekt) (string_of_sub_resource `acl) 
  in  
  let headers = [ "Date", date ; xml_content_type_header; authorization_header ] in
  let xml = xml_of_access_control_policy acl in
  let body = `String (X.string_of_xml xml) in
  try_lwt
    lwt _ = HC.put ~headers ~body request_url in
    return `Ok
  with 
    | HC.Http_error (404,_,_) -> return `NotFound
    | HC.Http_error (_,_,body) -> error_msg body
