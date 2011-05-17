open Lwt 

let _ = 
  print_endline "Test for SQS" ; 
  Lwt_main.run (
    SQS.list_queues { Creds.aws_access_key_id = Keys.k; 
                       Creds.aws_secret_access_key = Keys.c }
    >>= function 
      | `Ok url -> List.iter print_endline url ; return ()
      | `Error e -> print_endline e ; return ())
