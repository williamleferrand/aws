module C = CalendarLib.Calendar 
module P = CalendarLib.Printer.CalendarPrinter

open Creds
open Http_method

let sprint = Printf.sprintf

let compare_fst (k1,v1) (k2,v2) = String.compare k1 k2
let sort_by_keys params = List.sort compare_fst params

let expires_format = "%FT%TZ"

let expires_minutes_from_now minutes =
  let now = C.now () in
  let minutes_from_now = C.Period.make 0 0 0 0 minutes 0 in
  let now_plus_minutes_from_now = C.add now minutes_from_now in
  P.sprint expires_format now_plus_minutes_from_now

(* compute the AWS SHA1 signature that to annotate a Query-style request *)
let signed_request
    ?(http_method=`GET) 
    ?(http_host="ec2.amazonaws.com") 
    ?(http_uri="/")
    ?(expires_minutes)
    creds 
    params  = 

  let params = 
    ("Version", "2010-08-31" ) ::
      ("SignatureVersion", "2") ::
      ("SignatureMethod", "HmacSHA1") ::
      ("AWSAccessKeyId", creds.aws_access_key_id) :: 
      params
  in

  let params = 
    match expires_minutes with
      | Some i -> ("Expires", expires_minutes_from_now i) :: params 
      | None -> params
  in

  let signature = 
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
    let hmac_sha1_encoder = Cryptokit.MAC.hmac_sha1 creds.aws_secret_access_key in
    let signed_string = Cryptokit.hash_string hmac_sha1_encoder string_to_sign in
    Util.base64 signed_string 
  in

  let params = ("Signature", signature) :: params in
  let params_s = String.concat "&" (Util.encode_key_equals_value params) in
  sprint "http://%s%s?%s" http_host http_uri params_s


module HC = Cohttp.Http_client
open Lwt
module X = Xml

exception Error of string

let item_of_xml = function
  | X.Element("item",_,[
    X.Element("regionName",_,[X.PCData name]);
    X.Element("regionEndpoint",_,[X.PCData endpoint])
  ]) -> name, endpoint
  | _ -> raise (Error "RegionInfo.item")

let describe_regions_response_of_xml = function
  | X.Element("DescribeRegionsResponse", _, kids) -> (
    match kids with
      | [_ ; X.Element ("regionInfo",_,items_x)] -> (
        List.map item_of_xml items_x
      )
      | _ -> raise (Error "DescribeRegionsResponse:[]")
  )
  | _ -> raise (Error "DescribeRegionsResponse")

let describe_regions creds =
  let request = signed_request creds ~expires_minutes:5 ["Action", "DescribeRegions" ] in
  lwt header, body = HC.get request in
  let xml = X.parse_string body in  
  return (describe_regions_response_of_xml xml)

