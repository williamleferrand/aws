open Lwt

open SDB

let creds = { Creds.aws_access_key_id = Keys.k; 
                Creds.aws_secret_access_key = Keys.c } 

let list_domains () = 
  Lwt_main.run 
    (SDB.list_domains creds ()
     >>= function 
       | `Ok l -> List.iter print_endline l ; return ()
       | `Error msg -> Printf.printf "Panic: %s\n" msg; return ()) 

let create_domain name = 
  Lwt_main.run 
    (SDB.create_domain creds name
     >>= function 
       | `Ok -> return ()
       | `Error msg -> Printf.printf "Panic: %s\n" msg; return ())

let delete_domain name = 
  Lwt_main.run 
    (SDB.delete_domain creds name
     >>= function 
       | `Ok -> return ()
       | `Error msg -> Printf.printf "Panic: %s\n" msg; return ())

let get_attributes domain item = 
   Lwt_main.run 
     (SDB.get_attributes ~encoded:false creds domain item
      >>= function 
        | `Ok l -> List.iter (fun (n, v) -> Printf.printf "%s -> %s\n" n v) l; return ()
       | `Error msg -> Printf.printf "Panic: %s\n" msg; return ())

let _ = 
  get_attributes Sys.argv.(1) Sys.argv.(2)
