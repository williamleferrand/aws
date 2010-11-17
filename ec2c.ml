(* ec2 command line client *)
open Lwt

let describe_regions creds () =
  lwt regions = EC2.describe_regions creds in
  List.iter (fun (id, name) -> Printf.printf "%s\t%s\n" id name) regions;
  return 0

let describe_spot_price_history creds () =
  lwt history = EC2.describe_spot_price_history creds in
  List.iter (
    fun h -> 
      Printf.printf "%s\t%s\t%0.2f%s\n" 
        h#instance_type
        h#product_description
        h#spot_price
        (Util.date_string_of_unixfloat h#timestamp)
  ) history;
  return 0

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

      | _ ->
        print_endline "unknown command";
        exit 1
  in

  let exit_code = Lwt_unix.run (command ()) in
  exit exit_code
