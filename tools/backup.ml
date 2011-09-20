(* Backuping tools *)

open Lwt

(* misc tools *******************************************************************************************)

let display fmt = Printf.ksprintf print_endline fmt


(* backup logic *****************************************************************************************)

let rec read_and_store ?(token=None) creds oc sdb_domain () = 
    SDB.select ~token creds ("select * from " ^ sdb_domain)
      >>= function 
        | `Ok (elements, token) -> 
          (display "> iterating over the domain"; 
           List.iter
             (fun (name, attrs) -> 
               Printf.fprintf oc "--name %s" name ; 
               List.iter 
                 (fun (key, value_option) ->
                   match value_option with 
                       Some value -> Printf.fprintf oc "%s %s" key value
                     | None -> Printf.fprintf oc "%s" key) attrs)
             elements ;
            match token with 
                None -> display "> loading domain %s done" sdb_domain ; return () 
              | Some _ as token -> read_and_store ~token creds oc sdb_domain ())
        | `Error (s1, s2) -> 
          display "> error %s %s while selecting over domain %s, waiting for 5 secs and retrying" s1 s2 sdb_domain ; 
          Lwt_unix.sleep 5.0 >>= read_and_store ~token creds oc sdb_domain

let backup () = 
  let sdb_domain = Sys.argv.(1) in
  let bucket = Sys.argv.(2) in 
  let aws_access_key_id = Sys.argv.(3) in 
  let aws_secret_access_key = Sys.argv.(4) in
  let creds = 
    {
      Creds.aws_access_key_id ;
      Creds.aws_secret_access_key ;
    } in
  let tmp, oc = Filename.open_temp_file "aws" "backup" in 
  let objekt = Printf.sprintf "backup_%s_%f" sdb_domain (Unix.gettimeofday ()) in
  (try_lwt
    read_and_store creds oc sdb_domain ()
  finally (close_out oc; return ()))
  >>= function _ -> S3.put_object creds `US_WEST_1 ~bucket ~objekt ~body:(`File tmp)
  >>= function
    | `Ok -> display "> upload done; exiting"; return () 
    | `AccessDenied -> display "> access denied"; return ()
    | `Error e -> display "> error %s" e ; return ()
    | `PermanentRedirect _ -> display "> redirect"; return ()


(* usage ************************************************************************************************)

let usage () = 
  display "./backup sdb_domain s3domain" 

(* main *************************************************************************************************)

let _ = 
  display "> sdb backup" ; 
  if Array.length Sys.argv < 2 then
    usage ()
  else
    Lwt_main.run (backup ())
