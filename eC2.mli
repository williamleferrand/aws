val describe_regions : 
  ?expires_minutes:int -> 
  Creds.t -> 
  (string * string) list Lwt.t
(* return a list of region identifiers and their associated api url *)

type instance_type = [
| `m1_small 
| `m1_large 
| `m1_xlarge 
| `c1_medium 
| `c1_xlarge 
| `m2_xlarge 
| `m2_2xlarge 
| `m2_4xlarge 
| `cc1_4xlarge
| `cg1_4xlarge 
| `t1_micro
]
val string_of_instance_type : instance_type -> string
val instance_type_of_string : string -> instance_type option

val describe_spot_price_history : 
  ?expires_minutes:int ->
  ?region:string ->
  ?instance_type:instance_type ->
  Creds.t -> < 
    instance_type : string; 
    product_description : string; 
    spot_price : float; 
    timestamp : float 
  > list Lwt.t

type instance_state = [
| `pending 
| `running 
| `shutting_down 
| `terminated 
| `stopping 
| `stopped
| `problematic
]

val string_of_instance_state : instance_state -> string

val terminate_instances : 
  ?expires_minutes:int ->
  ?region:string ->
  Creds.t ->
  string list ->
  [> `Error of string
  | `Ok of < 
      current_state : instance_state; 
      instance_id : string;
      previous_state : instance_state 
     > list 
  ] Lwt.t


type instance = <
  id : string;
  ami_launch_index : int; 
  architecture_opt : string option;
  placement_availability_zone_opt : string option; 
  dns_name_opt : string option;
  placement_group_opt : string option; 
  image_id : string; 
  instance_type : instance_type;
  ip_address_opt : string option; 
  kernel_id_opt : string option;
  key_name_opt : string option; 
  launch_time : float;
  lifecycle_opt : string option; 
  private_dns_name_opt : string option;
  private_ip_address_opt : string option; 
  ramdisk_id_opt : string option;
  reason_opt : string option; 
  root_device_name_opt : string option; 
  root_device_type : string; 
  state : instance_state;
  virtualization_type_opt : string option;
  monitoring : string
>

type reservation = < 
  id : string;
  groups : string list; 
  owner_id : string; 
  instances : instance list
>

val describe_instances : 
  ?expires_minutes:int ->
  ?region:string ->
  Creds.t ->
  string list ->
  [> `Error of string | `Ok of reservation list ] Lwt.t

val run_instances :
  ?expires_minutes:int -> 
  ?key_name:string ->
  ?placement_availability_zone:string ->
  ?region:string ->
  ?placement_group:string ->
  ?instance_type:instance_type ->
  Creds.t ->
  image_id:string ->
  min_count:int ->
  max_count:int ->
  [> `Error of string | `Ok of reservation ] Lwt.t

  

type spot_instance_request_type = [`OneTime | `Persistent]
val string_of_spot_instance_request_type : spot_instance_request_type -> string


type spot_instance_request_state = [ `Active | `Open | `Closed | `Cancelled | `Failed ]
val string_of_spot_instance_request_state : spot_instance_request_state -> string

type spot_instance_request = {
  sir_spot_price : float ;
  sir_instance_count : int option;
  sir_type : spot_instance_request_type option;
  sir_valid_from : float option;
  sir_valid_until: float option;
  sir_launch_group : string option;
  sir_image_id : string ;
  sir_security_group : string option ;
  sir_user_data : string option;
  sir_instance_type : instance_type option;
  sir_kernel_id : string option;
  sir_ramdisk_id : string option;
  sir_availability_zone : string option;
  sir_monitoring_enabled : bool option;
  sir_key_name : string option;
  sir_availability_zone_group : string option;
  sir_placement_group : string option; 
  (* as distinct from LaunchGroup; assuming this works, although not documented *)
}  

val minimal_spot_instance_request : 
  spot_price:float ->
  image_id:string -> 
  spot_instance_request

type spot_instance_request_description = < 
  id : string; 
  instance_id_opt : string option;
  sir_type : spot_instance_request_type; 
  spot_price : float;
  state : spot_instance_request_state;
  image_id_opt : string option;
  key_name_opt : string option;
  groups : string list;
  placement_group_opt : string option;
>

val request_spot_instances : 
  ?region:string ->
  Creds.t ->
  spot_instance_request ->
  [> `Error of string | `Ok of spot_instance_request_description list ] Lwt.t

val describe_spot_instance_requests :
  ?region:string ->
  Creds.t ->
  string list ->
  [> `Error of string | `Ok of spot_instance_request_description list ] Lwt.t  
(* [describe_spot_instance_requests region ~return creds ids] returns
   a description of the spot instance requests associated with each of
   the id's in [ids]; when [ids] is an empty list, all the spot
   instance request descriptions are returned. *)

val cancel_spot_instance_requests :
  ?region:string ->
  Creds.t ->
  string list ->
  [> `Error of string | `Ok of (string * spot_instance_request_state) list ] Lwt.t  
(** return a list of spot instance request id's and their associated state *)
