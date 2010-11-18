(* ec2 command line client *)
open Lwt
open Printf

let describe_regions creds () =
  lwt regions = EC2.describe_regions creds in
  List.iter (fun (id, name) -> Printf.printf "%s\t%s\n" id name) regions;
  return 0

let describe_spot_price_history creds () =
  lwt history = EC2.describe_spot_price_history creds in
  List.iter (
    fun h -> 
      Printf.printf "%s\t%s\t%0.4f\t%0.3f\n" 
        h#instance_type
        h#product_description
        h#spot_price
        h#timestamp
  ) history;
  return 0

let terminate_instances ~region creds instance_ids () =
  lwt resp = EC2.terminate_instances ~region creds instance_ids in
  match resp with
    | `Ok killed -> 
      List.iter (
        fun i ->
          printf "%s\t%s\t%d\t%s\t%d\n" 
            i#instance_id
            i#previous_state#name
            i#previous_state#code
            i#current_state#name
            i#current_state#code
      ) killed;
      return 0
    | `Error msg -> print_endline msg; return 1

let string_of_opt = function
  | None -> "-"
  | Some s -> s

let print_reservation r =
  List.iter (
    fun instance ->
      printf "%s\t%s\t%s\t%s\t%s\n" 
        r#id 
        instance#id
        instance#image_id
        (string_of_opt instance#private_dns_name_opt)
        (string_of_opt instance#dns_name_opt)
  ) r#instances

let describe_instances ~region creds instance_ids () =
  lwt resp = EC2.describe_instances ~region creds instance_ids in
  match resp with
    | `Ok reservations ->
      List.iter print_reservation reservations; return 0
    | `Error msg -> print_endline msg; return 1

let run_instances 
    creds 
    ~key_name 
    ~region
    ~availability_zone 
    ~image_id 
    ~min_count 
    ~max_count () =

  lwt resp = EC2.run_instances 
    creds 
    ~key_name 
    ~region
    ~availability_zone 
    ~image_id
    ~min_count 
    ~max_count 
  in
  match resp with
    | `Ok reservation ->
      print_reservation reservation; return 0
    | `Error msg -> print_endline msg; return 1

let _ =
  let creds = 
    try 
      Util.creds_of_env () 
    with Failure msg -> 
      print_endline msg;
      exit 1
  in

  let command = 
    match Sys.argv with
      | [| _; "describe-regions" |] -> 
        describe_regions creds 

      | [| _; "describe-spot-price-history" |] ->
        describe_spot_price_history creds

      | [| _; "describe-instances"; region |] ->
        describe_instances creds ~region []

      | [| _; "run-instances"; region; availability_zone; key_name; image_id |] ->
        run_instances 
          ~region
          ~availability_zone
          ~key_name
          ~image_id 
          ~min_count:1 
          ~max_count:1 
          creds 

      | [| _; "terminate-instances"; region; instance_id |] ->
        terminate_instances creds ~region [instance_id]

      | _ -> (
        print_endline "unknown command";
        exit 1
      )
  in

  let exit_code = Lwt_unix.run (command ()) in
  exit exit_code
