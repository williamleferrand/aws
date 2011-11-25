open Lwt

let display fmt = Printf.ksprintf print_endline fmt

let update creds domain item key value =
  SDB.delete_attributes 
    creds
    domain 
    item
    [ (* key, Some value *) ]
    >>= function 
      | `Ok -> display "> done" ; return ()
      | `Error _ -> display "> error" ; return ()

(* main *)


let _ = 
  display "> update value "; 
  let domain = Sys.argv.(1) in
  let aws_access_key_id = Sys.argv.(2) in 
  let aws_secret_access_key = Sys.argv.(3) in
  let item = Sys.argv.(4) in 
  let key = Sys.argv.(5) in 
  let value = Sys.argv.(6) in
  let creds = 
    {
      Creds.aws_access_key_id ;
      Creds.aws_secret_access_key ;
    } in

  Lwt_main.run (update creds domain item key value)
