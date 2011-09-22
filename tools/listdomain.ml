open Lwt

let display fmt = Printf.ksprintf print_endline fmt


let rec list_domain ?(token=None) creds domain () = 
    SDB.select ~token creds ("select * from " ^ domain)
    >>= function 
      | `Ok (elements, token) -> 
        (
          List.iter 
            (fun (name, attrs) -> 
              display "> name %s" name; 
              List.iter (fun (key, value) ->
                match value with 
                    None -> display "    %s" key 
                  | Some value -> display "    %s %s" key value) attrs 
            ) elements ;
          match token with 
              None -> display "> loading domain %s done" domain ; return () 
            | Some _ as token -> list_domain ~token creds domain ())
      | `Error _ -> display "> error" ; return ()  

let delete_domain creds domain = 
  SDB.delete_domain creds domain
  >>= function `Ok -> return () 
    | `Error (s, s') -> display "error %s %s" s s' ; return ()

let create_domain creds domain = 
  SDB.create_domain creds domain
  >>= function `Ok -> return () 
    | `Error (s, s') -> display "error %s %s" s s' ; return ()

let _ = 
  display "> list domain"; 
  let domain = Sys.argv.(1) in
  let aws_access_key_id = Sys.argv.(2) in 
  let aws_secret_access_key = Sys.argv.(3) in
  let creds = 
    {
      Creds.aws_access_key_id ;
      Creds.aws_secret_access_key ;
    } in

  Lwt_main.run (list_domain creds domain ())
(*
  Lwt_main.run (create_domain creds domain)
*)   
