open Lwt 

(*
let _ = 
  print_endline "Test for SQS" ; 
  Lwt_main.run (
    SQS.list_queues { Creds.aws_access_key_id = Keys.k; 
                       Creds.aws_secret_access_key = Keys.c }
    >>= function 
      | `Ok url -> List.iter print_endline url ; return ()
      | `Error e -> print_endline e ; return ())
*)

let queue_url = "/212838181503/adpagesss"

open SQS

let _ = 
  let creds = { Creds.aws_access_key_id = Keys.k; 
                Creds.aws_secret_access_key = Keys.c } in
  Lwt_main.run (
    SQS.receive_message ~max_number_of_messages:2 creds queue_url
    >>= function
      | `Ok l ->
        Lwt_list.iter_s (fun m ->
          SQS.delete_message creds queue_url m.receipt_handle
          >>= fun _ -> return () 
        ) l ;
        
      | `Error _ -> return () 
  )
