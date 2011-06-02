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

let queue_url = "/212838181503/adclicks"

open SQS

let _ = 
  let creds = { Creds.aws_access_key_id = Keys.k; 
                Creds.aws_secret_access_key = Keys.c } in
  Lwt_main.run (
    SQS.receive_message ~encoded:false ~max_number_of_messages:2 creds queue_url
    >>= function
      | `Ok l ->
        Printf.printf "We read %d messages from the queue\n" (List.length l); 
        Lwt_list.iter_s (fun m -> 
          print_endline m.body ;
          SQS.delete_message creds queue_url m.receipt_handle
          >>= fun _ -> return () 
        ) l ;
        
      | `Error _ -> return () 
  )

(* http://document.issuu.com.s3.amazonaws.com//txt/full.txt.gz *)
(*
let doc_id = "110511121239-51a22ec13fc5428f92a2ae5342ad688d"

let default_region = ref `US_EAST_1

let rec get_from_s3 creds () = 
  let objekt = (Printf.sprintf "%s/txt/full.txt.gz" doc_id) in
  
   S3.get_object_s 
    (Some creds)
    !default_region
    ~bucket:"document.issuu.com"
    ~objekt
    >>= function
      | `Ok s -> print_endline "we have the body" ; return () 
      | `NotFound -> print_endline "not_found" ; return ()
      | `Error e -> print_endline e ; return ()
      | `PermanentRedirect (Some r) -> default_region := r ;  print_endline "redirect" ; get_from_s3 creds ()
      | `PermanentRedirect None -> default_region := `US_EAST_1 ; print_endline "redirect to default"; get_from_s3 creds ()


let _ = 
  
  let creds = { Creds.aws_access_key_id = Keys.k; 
                Creds.aws_secret_access_key = Keys.c } in
   
  Lwt_main.run
    (
      get_from_s3 creds ())
*)
