let compare_fst (k1,v1) (k2,v2) = String.compare k1 k2
let sort_by_keys params = List.sort compare_fst params

let sprint = Printf.sprintf

type http_method = [ `POST | `GET ]
let string_of_http_method = function
  | `POST -> "POST"
  | `GET  -> "GET"

(* compute the AWS SHA1 signature that to annotate a Query-style request *)
let signature ~http_method ~http_host ~http_uri ~aws_secret_access_key params  = 
  let sorted_params = sort_by_keys params in
  let key_equals_value = Util.encode_key_equals_value sorted_params in
  let uri_query_component = String.concat "&" key_equals_value in
  let string_to_sign = String.concat "\n" [ 
    string_of_http_method http_method ;
    String.lowercase http_host ;
    http_uri ;
    uri_query_component 
  ]
  in

  let hmac_sha1_encoder = Cryptokit.MAC.hmac_sha1 aws_secret_access_key in
  let signed_string = Cryptokit.hash_string hmac_sha1_encoder string_to_sign in
  Util.base64 signed_string


type http_protocol = [`HTTP | `HTTPS]

let string_of_http_protocol = function
  | `HTTP  -> "http"
  | `HTTPS -> "https"

let signature_version = "SignatureVersion", "2"
let signature_method = "SignatureMethod", "HmacSHA1"

let expires_format = "%FT%TZ"

module C = CalendarLib.Calendar 
module P = CalendarLib.Printer.CalendarPrinter

let expires_minutes_from_now minutes =
  let now = C.now () in
  let minutes_from_now = C.Period.make 0 0 0 0 minutes 0 in
  let now_plus_minutes_from_now = C.add now minutes_from_now in
  P.sprint expires_format now_plus_minutes_from_now


(* augment a request, represented as a key-value list [params], with a
   signature *)
let sign 
    ~http_method 
    ~http_protocol 
    ~http_host 
    ~http_uri 
    ~aws_access_key_id 
    ~aws_secret_access_key 
    ~request_expiration_minutes
    params =
  
  let expires_minutes_from_now_s = 
    expires_minutes_from_now request_expiration_minutes
  in

  let params = 
    ("AWSAccessKeyId", aws_access_key_id) :: 
      ("Expires", expires_minutes_from_now_s) ::
      signature_version :: 
      signature_method :: 
      params
  in
  
  let signature = signature 
    ~http_method 
    ~http_host 
    ~http_uri 
    ~aws_secret_access_key 
    params 
  in

  let params_with_signature = ("Signature", signature) :: params in
  let params_s = String.concat "&" (Util.encode_key_equals_value params_with_signature) in
  let http_protocol_s = string_of_http_protocol http_protocol in
  (* compose signed request url *)
  sprint "%s://%s%s?%s" http_protocol_s http_host http_uri params_s

type config = {
  http_method : http_method;
  http_protocol : http_protocol;
  http_host : string ;
  http_uri : string ;
  creds : Creds.t ;
  api_version : string ;
  request_expiration_minutes : int ;
}

let test_auth creds =
  let http_protocol = `HTTP in
  let http_method = `GET in
  let http_host = "ec2.amazonaws.com" in
  let http_uri = "/"  in
  let ec2_api_version = "Version", "2010-08-31" in
  let params = ["Action", "DescribeRegions"; ec2_api_version ] in
  let request_expiration_minutes = 5 in
  sign 
    ~http_method 
    ~http_protocol 
    ~http_host 
    ~http_uri 
    ~aws_access_key_id:creds.Creds.aws_access_key_id
    ~aws_secret_access_key:creds.Creds.aws_secret_access_key 
    ~request_expiration_minutes
    params

let c creds = {
  http_protocol = `HTTP;
  http_method = `GET;
  http_host = "ec2.amazonaws.com";
  http_uri = "/";
  api_version = "2010-08-31";
  request_expiration_minutes = 5;
  creds = creds;
}
  
let sign config =
  let creds = config.creds in
  let sign_with_fixed = sign
    ~http_method:config.http_method
    ~http_protocol:config.http_protocol
    ~http_host:config.http_host
    ~http_uri:config.http_uri
    ~aws_access_key_id:creds.Creds.aws_access_key_id
    ~aws_secret_access_key:creds.Creds.aws_secret_access_key
    ~request_expiration_minutes:config.request_expiration_minutes
  in
  fun params ->
    (* augment params with api version *)
    let params = ("Version", config.api_version) :: params in
    sign_with_fixed params

module HC = Cohttp.Http_client
open Lwt

let call config =
  let sign_with_config = sign config in
  fun action ->
    let request = sign_with_config ["Action", action] in
    lwt header, body = HC.get request in
    return (Xml.parse_string body)

(*
let test_ec2 () =
  let sign = sign c in
  let request = sign ["Action", "DescribeRegions"] in
  (* sign ["Action", "DescribeImages"] *)
  Lwt_unix.run (
    lwt header, body = HC.get request in
    return body
  )
*)
