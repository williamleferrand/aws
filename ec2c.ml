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

let terminate_instances creds instance_ids () =
  lwt resp = EC2.terminate_instances creds instance_ids in
  match resp with
    | `Ok killed -> 
      List.iter (
        fun terminated_instance ->
          ()
      ) killed;
      return 0
    | `Error msg ->
      print_endline msg;
      return 1

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

      | cmd ->
        let argc = Array.length Sys.argv in
        if argc > 2 && Sys.argv.(1) = "terminate-instances" then
          (* all remaining arguments are instance ids *)
          let instance_ids = Array.to_list (Array.sub Sys.argv 2 (argc - 2)) in
          terminate_instances creds instance_ids
        else (
          print_endline "unknown command";
          exit 1
        )
  in

  let exit_code = Lwt_unix.run (command ()) in
  exit exit_code
