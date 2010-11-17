module C = CalendarLib.Calendar 
module P = CalendarLib.Printer.CalendarPrinter

open Creds
open Http_method

let sprint = Printf.sprintf

let compare_fst (k1,v1) (k2,v2) = String.compare k1 k2
let sort_by_keys params = List.sort compare_fst params

let default_expires_minutes = 5

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

(* describe regions *)
let item_of_xml = function
  | X.E("item",_,[
    X.E("regionName",_,[X.P name]);
    X.E("regionEndpoint",_,[X.P endpoint])
  ]) -> name, endpoint
  | _ -> raise (Error "RegionInfo.item")

let describe_regions_response_of_xml = function
  | X.E("DescribeRegionsResponse", _, kids) -> (
    match kids with
      | [_ ; X.E ("regionInfo",_,items_x)] -> (
        List.map item_of_xml items_x
      )
      | _ -> raise (Error "DescribeRegionsResponse:[]")
  )
  | _ -> raise (Error "DescribeRegionsResponse")

let describe_regions ?(expires_minutes=default_expires_minutes) creds =
  let request = signed_request creds ~expires_minutes
    ["Action", "DescribeRegions" ] in
  lwt header, body = HC.get request in
  let xml = X.parse_string body in  
  return (describe_regions_response_of_xml xml)

(* describe spot price history *)
(*
<DescribeSpotPriceHistoryResponse xmlns="http://ec2.amazonaws.com/doc/2010-08-31/">
  <requestId>59dbff89-35bd-4eac-99ed-be587EXAMPLE</requestId> 
  <spotPriceHistorySet>
    <item>
      <instanceType>m1.small</instanceType>
      <productDescription>Linux/UNIX</productDescription>
      <spotPrice>0.287</spotPrice>
      <timestamp>2009-12-04T20:56:05.000Z</timestamp>
    </item>
    <item>
      <instanceType>m1.small</instanceType>
      <productDescription>Windows</productDescription>
      <spotPrice>0.033</spotPrice>
      <timestamp>2009-12-04T22:33:47.000Z</timestamp>
    </item>
  </ spotPriceHistorySet>
</DescribeSpotPriceHistoryResponse>
*)

let item_of_xml = function 
  | X.E ("item",_,[
    X.E ("instanceType",_,[X.P instance_type]);
    X.E ("productDescription",_,[X.P product_description]);
    X.E ("spotPrice",_,[X.P spot_price_s]);
    X.E ("timestamp",_,[X.P timestamp_s])
  ]) ->

    let spot_price = float_of_string spot_price_s in
    let timestamp = Util.parse_amz_date_string timestamp_s in
    (object 
      method instance_type = instance_type
      method product_description = product_description
      method spot_price = spot_price 
      method timestamp = timestamp
     end)

  | _ -> raise (Error (String.concat "." [
    "DescribeSpotPriceHistoryResponse";
    "spotPriceHistorySet";
    "item"
  ]))
      

let describe_spot_price_history_of_xml = function
  | X.E ("DescribeSpotPriceHistoryResponse",_,kids) -> (
    match kids with
      | [_ ; X.E("spotPriceHistorySet",_,kids) ] ->
        List.map item_of_xml kids

      | _ ->
        raise (
          Error ("DescribeSpotPriceHistoryResponse." ^ 
            "spotPriceHistorySet")
        )
  )
  | _ ->
    raise (Error "DescribeSpotPriceHistoryResponse")

let describe_spot_price_history ?(expires_minutes=default_expires_minutes) creds =
  let request = signed_request creds ~expires_minutes 
    ["Action", "DescribeSpotPriceHistory" ] in
  lwt header, body = HC.get request in
  let xml = X.parse_string body in
  return (describe_spot_price_history_of_xml xml)

