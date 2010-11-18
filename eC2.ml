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
    ?region
    ?(http_method=`GET) 
    ?(http_uri="/")
    ?expires_minutes
    creds 
    params  = 

  let http_host =
    match region with
      | Some r -> sprint "ec2.%s.amazonaws.com" r
      | None -> "ec2.amazonaws.com"
  in

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
class state code name =
object
  method code : int = code
  method name : string = name
end

let state_of_xml = function
  | [ X.E ("code",_,[X.P code_s]);
      X.E ("name",_,[X.P name])
    ] ->
    new state (int_of_string code_s) name
  | _ ->
    raise (Error "state")

let item_of_xml = function
  | X.E ("item",_, [
    X.E ("instanceId",_,[X.P instance_id]);
    X.E ("currentState",_,c_state_x);
    X.E ("previousState",_,p_state_x)
  ]) -> 
    let current_state = state_of_xml c_state_x in
    let previous_state = state_of_xml p_state_x in
    (object 
      method instance_id = instance_id
      method current_state = current_state
      method previous_state = previous_state
     end)
    
  | _ ->
    raise (Error "TerminateInstancesResponse.instancesSet.item")
      

let terminate_instances_of_xml = function
  | X.E ("TerminateInstancesResponse",_, [
    _request_id_x;
    X.E ("instancesSet",_, items)
  ]) ->
    List.map item_of_xml items
  | _ -> raise (Error "TerminateInstancesResponse")

let error_msg body =
  match X.xml_of_string body with
    | X.E ("Response",_,(X.E ("Errors",_,[X.E ("Error",_,[
      X.E ("Code",_,[X.P code]);
      X.E ("Message",_,[X.P message])
    ])]))::_) ->
      `Error message
  
  | _ -> raise (Error "Response.Errors.Error")

let instance_id_args instance_ids =
  Util.list_map_i (
    fun i instance_id ->
      sprint "InstanceId.%d" (i+1), instance_id
  ) instance_ids

let terminate_instances ?expires_minutes ?region creds instance_ids =
  let args = instance_id_args instance_ids in
  let request = signed_request creds ?region ?expires_minutes
    (("Action", "TerminateInstances") :: args) 
  in
  try_lwt
    lwt header, body = HC.get request in
    let xml = X.xml_of_string body in
    return (`Ok  (terminate_instances_of_xml xml))
  with 
    | HC.Http_error (_,_,body) ->
      return (error_msg body)

(* describe instances *)
type instance = <
  id : string;
  ami_launch_index : int; 
  architecture : string;
  availability_zone : string; 
  dns_name_opt : string option;
  group_name_opt : string option; 
  image_id : string; 
  instance_type : string;
  ip_address_opt : string option; 
  kernel_id : string;
  key_name : string; 
  launch_time : float;
  lifecycle_opt : string option; 
  private_dns_name_opt : string option;
  private_ip_address_opt : string option; 
  ramdisk_id_opt: string option;
  reason_opt : string option; 
  root_device_name_opt : string option; 
  root_device_type : string; 
  state : state;
  virtualization_type : string ;
  monitoring : string
>

type reservation = < 
  id : string;
  groups : string list; 
  owner_id : string; 
  instances : instance list
>

let group_of_xml = function
  | X.E("item",_,[X.E("groupId",_,[X.P group])]) -> group
  | _ -> 
    raise (Error (
      String.concat "." [
        "DescribeInstancesResponse";
        "reservationSet";
        "item";
        "groupSet";
        "item"]
    ))
           
let placement_of_xml = function
  | [X.E("availabilityZone",_,[X.P availability_zone]);
     X.E("groupName",_,group_name_x) ] ->
    let group_name_opt = 
      match group_name_x with
        | [] -> None
        | [X.P group_name] -> Some group_name
        | _ ->
          raise (Error ("DescribeInstancesResponse.reservationSet...placement.groupName"))
    in
    availability_zone, group_name_opt 
  | _ ->
    raise (Error ("DescribeInstancesResponse.reservationSet...placement"))

let find_kids e_list k =
  try
    let el = List.find (function
      | X.E( name, _, kids ) -> name = k
      | X.P _ -> false
    ) e_list in
    let kids = 
      match el with
        | X.E( name, _, kids ) -> kids
        | X.P _ -> assert false
    in
    Some kids
  with Not_found -> 
    None

let find_kids_else_error e_list k =
  match find_kids e_list k with 
    | None -> raise (Error k)
    | Some kids -> kids

let find_p_kid e_list k =
  match find_kids e_list k with
    | None -> None
    | Some [X.P p_kid] -> Some p_kid
    | _ -> None

let find_e_kid e_list k =
  match find_kids e_list k with
    | None -> None
    | Some [e] -> Some e
    | _ -> None

let find_p_kid_else_error e_list k =
  match find_p_kid e_list k with
    | None -> raise (Error k)
    | Some p -> p

let find_e_kid_else_error e_list k =
  match find_e_kid e_list k with
    | None -> raise (Error k)
    | Some e -> e

let instance_of_xml = function 
  | X.E("item",_,kids) ->
    
    let fp = find_p_kid_else_error kids in
    let fpo = find_p_kid kids in

    let id = fp "instanceId" in
    let image_id = fp "imageId" in
    let state_x = find_kids_else_error kids "instanceState" in
    let private_dns_name_opt = fpo "privateDnsName" in
    let dns_name_opt = fpo "dnsName" in
    let key_name = fp "keyName" in
    let ami_launch_index_s = fp "amiLaunchIndex" in
    let instance_type = fp "instanceType" in
    let launch_time_s = fp "launchTime" in
    let placement_x = find_kids_else_error kids "placement" in
    let kernel_id = fp "kernelId" in
    let virtualization_type = fp "virtualizationType" in
    let private_ip_address_opt = fpo "privateIpAddress" in
    let ip_address_opt = fpo "ipAddress" in
    let architecture = fp "architecture" in
    let root_device_type = fp "rootDeviceType" in
    let root_device_name_opt = find_p_kid kids "rootDeviceName" in
    let reason_opt = fpo "reason" in
    let ramdisk_id_opt = fpo "ramdiskId" in
    let lifecycle_opt = fpo "instanceLifecycle" in
    let monitoring = 
      match find_kids kids "monitoring" with
        | Some [X.E ("state",_,[X.P monitoring_state])] -> monitoring_state
        | _ -> raise (Error "monitoring")
    in

    (* TODO: 
       product_code
       block_device_mapping
       client_token
       tags
       product_codes
       block_device_mapping 
       spot_instance_request_id
    *)

    let state = state_of_xml state_x in
    let ami_launch_index = int_of_string ami_launch_index_s in
    let launch_time = Util.parse_amz_date_string launch_time_s in
    let availability_zone, group_name_opt = placement_of_xml placement_x in
    (object 
      method id = id
      method image_id = image_id
      method state = state
      method private_dns_name_opt = private_dns_name_opt
      method dns_name_opt = dns_name_opt
      method reason_opt = reason_opt
      method key_name = key_name
      method ami_launch_index = ami_launch_index
      method instance_type = instance_type
      method launch_time = launch_time
      method availability_zone = availability_zone
      method group_name_opt = group_name_opt
      method kernel_id = kernel_id
      method ramdisk_id_opt = ramdisk_id_opt
      method private_ip_address_opt = private_ip_address_opt
      method ip_address_opt = ip_address_opt
      method architecture = architecture
      method root_device_type = root_device_type
      method root_device_name_opt = root_device_name_opt
      method lifecycle_opt = lifecycle_opt
      method virtualization_type = virtualization_type
      method monitoring = monitoring
     end)
  | _ ->
    raise (Error "DescribeInstancesResponse.reservationSet.item.instancesSet")

let reservation_of_xml = function
  | [X.E("reservationId",_,[X.P reservation_id]);
     X.E("ownerId",_,[X.P owner_id]);
     X.E("groupSet",_,groups_x);
     X.E("instancesSet",_,instances_x)
    ] ->
    let groups = List.map group_of_xml groups_x in
    let instances = List.map instance_of_xml instances_x in
    (object
      method id = reservation_id
      method owner_id = owner_id
      method groups = groups 
      method instances = instances
     end)
  | _ -> raise (Error "DescribeInstancesResponse.reservationSet.item.*")

let reservation_item_of_xml = function
  | X.E("item",_,reservation_x) -> reservation_of_xml reservation_x
  | _ -> raise (Error "DescribeInstancesResponse.reservationSet.item")

let describe_instances_of_xml = function
  | X.E("DescribeInstancesResponse",_,kids) -> (
    match kids with
      | [_; X.E("reservationSet",_,reservation_items)] ->
        List.map reservation_item_of_xml reservation_items
      | _ ->
        raise (Error "DescribeInstancesResponse.reservationSet")
  )
  | _ ->
    raise (Error "DescribeInstancesResponse")
    
let describe_instances ?expires_minutes ?region creds instance_ids =
  let args = instance_id_args instance_ids in
  let request = signed_request creds ?expires_minutes ?region
    (("Action", "DescribeInstances") :: args)
  in
  try_lwt 
    lwt header, body = HC.get request in
    let xml = X.xml_of_string body in
    return (`Ok (describe_instances_of_xml xml))
  with
    | HC.Http_error (_,_,body) ->
      return (error_msg body)

(* run instances *)
let run_instances_of_xml = function
  | X.E("RunInstancesResponse",_, reservation_x) -> 
    reservation_of_xml reservation_x
  | _ ->
    raise (Error "RunInstancesResponse")

let augment_opt f x = function
  | None -> x
  | Some y -> (f y) :: x

let run_instances 
    ?expires_minutes 
    ?key_name 
    ?availability_zone
    ?region
    creds 
    ~image_id 
    ~min_count 
    ~max_count =
  let args = [
    "Action", "RunInstances" ;
    "MinCount", string_of_int min_count ;
    "MaxCount", string_of_int max_count ;
    "ImageId", image_id 
  ]
  in
  let args = augment_opt 
    (fun az -> "Placement.AvailabilityZone", az) args availability_zone in
  let args = augment_opt (fun kn -> "KeyName", kn) args key_name in
  let request = signed_request creds ?expires_minutes ?region args in
  try_lwt 
    lwt header, body = HC.get request in
    let xml = X.xml_of_string body in
    return (`Ok (run_instances_of_xml xml))
  with
    | HC.Http_error (_,_,body) ->
      return (error_msg body)
