open Lwt

open SDB

let creds = { Creds.aws_access_key_id = Keys.k; 
                Creds.aws_secret_access_key = Keys.c } in

Lwt_main.run 
  (SDB.list_domains creds () 
   >>= function 
     | `Ok l -> Printf.printf "Number of domains: %d\n" (List.length l) ;
       List.iter print_endline l ; return ()
     | `Error msg -> Printf.printf "Panic: %s\n" msg; return ())
