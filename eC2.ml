module C = CalendarLib.Calendar 
module P = CalendarLib.Printer.CalendarPrinter

open Creds
open Http_method

let sprint = Printf.sprintf

let compare_fst (k1,v1) (k2,v2) = String.compare k1 k2
let sort_by_keys params = List.sort compare_fst params

let datetime_format = "%FT%TZ"

let expires_minutes_from_now minutes =
  let now = C.now () in
  let minutes_from_now = C.Period.make 0 0 0 0 minutes 0 in
  let now_plus_minutes_from_now = C.add now minutes_from_now in
  P.sprint datetime_format now_plus_minutes_from_now

let now_as_string () =
  P.sprint datetime_format (C.now ())

(* compute the AWS SHA1 signature that to annotate a Query-style request *)
let signed_request
    ?(http_method=`GET) 
    ?(http_host="ec2.amazonaws.com") 
    ?(http_uri="/")
    ?expires_minutes
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
      | None -> ("Timestamp", now_as_string ()) :: params
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
  | _ -> raise (Error "DescribeRegionsResponse.RegionInfo.item")

let describe_regions_response_of_xml = function
  | X.E("DescribeRegionsResponse", _, kids) -> (
    match kids with
      | [_ ; X.E ("regionInfo",_,items_x)] -> (
        List.map item_of_xml items_x
      )
      | _ -> raise (Error "DescribeRegionsResponse.regionInfo")
  )
  | _ -> raise (Error "DescribeRegionsResponse")

let describe_regions ?expires_minutes creds =
  let request = signed_request creds ?expires_minutes
    ["Action", "DescribeRegions" ] in
  lwt header, body = HC.get request in
  let xml = X.parse_string body in  
  return (describe_regions_response_of_xml xml)

(* describe spot price history *)
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

let describe_spot_price_history ?expires_minutes creds =
  let request = signed_request creds ?expires_minutes
    ["Action", "DescribeSpotPriceHistory" ] 
  in
  lwt header, body = HC.get request in
  let xml = X.parse_string body in
  return (describe_spot_price_history_of_xml xml)

(* terminate instances *)
(*
<TerminateInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2010-08-31/">
  <instancesSet>
    <item>
      <instanceId>i-3ea74257</instanceId>
      <currentState>
        <code>32</code>
        <name>shutting-down</name>
      </currentState>
      <previousState>
        <code>16</code>
        <name>running</name>
      </previousState>
    </item>
  </instancesSet>
</TerminateInstancesResponse>
*)

class state code name =
object
  method code : int = code
  method name : string = name
end

let item_of_xml = function
  | X.E ("item",_, [
    X.E ("instanceId",_,[X.P instance_id]);
    X.E ("currentState",_,[
      X.E ("code",_,[X.P c_code_s]);
      X.E ("name",_,[X.P c_name])
    ]);
    X.E ("previousState",_,[
      X.E ("code",_,[X.P p_code_s]);
      X.E ("name",_,[X.P p_name])
    ])
  ]) -> 
    let current_state = new state (int_of_string c_code_s) c_name in
    let previous_state = new state (int_of_string p_code_s) p_name in
    (object 
      method instance_id = instance_id
      method current_state = current_state
      method previous_state = previous_state
     end)
    
  | _ ->
    raise (Error "TerminateInstancesResponse.instanceSet.item")
      

let terminate_instances_of_xml = function
  | X.E ("TerminateInstancesResponse",_, [X.E ("instanceSet",_, items)]) ->
    List.map item_of_xml items
  | _ -> raise (Error "TerminateInstancesResponse")

(*
<Response><Errors><Error><Code>InvalidInstanceID.Malformed</Code><Message>Invalid id: "i2"</Message></Error></Errors><RequestID>3804d5da-376b-4192-9de4-7e9a05f3d766</RequestID></Respon
*)


let error_msg body =
  match X.xml_of_string body with
    | X.E ("Response",_,(X.E ("Errors",_,[X.E ("Error",_,[
      X.E ("Code",_,[X.P code]);
      X.E ("Message",_,[X.P message])
    ])]))::_) ->
      `Error message
  
  | _ -> raise (Error "Response.Errors.Error")

let terminate_instances ?expires_minutes creds instance_ids =
  let args = Util.list_map_i (
    fun i instance_id ->
      sprint "InstanceId.%d" (i+1), instance_id
  ) instance_ids
  in
  let request = signed_request creds ?expires_minutes
    (("Action", "TerminateInstances") :: args) 
  in
  try_lwt
    lwt header, body = HC.get request in
    let xml = X.parse_string body in
    return (`Ok  (terminate_instances_of_xml xml))
  with 
    | HC.Http_error (_,_,body) ->
      return (error_msg body)
