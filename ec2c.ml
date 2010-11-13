(* ec2 command line client *)
open Lwt

let _ =
  let creds = 
    try 
      Util.creds_of_env () 
    with Failure msg -> 
      print_endline msg;
      exit 1
  in

  Lwt_unix.run (
    lwt regions = EC2.describe_regions creds in
    List.iter (fun (id, name) -> Printf.printf "%s\t%s\n" id name) regions;
    return ()
  )           
