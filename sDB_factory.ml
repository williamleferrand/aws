(* SDB API *)
(* william@corefarm.com *)

module Make = functor (HC : Sigs.HTTP_CLIENT) -> 
struct 

  module C = CalendarLib.Calendar 
  module P = CalendarLib.Printer.CalendarPrinter
  module X = Xml


  open Lwt
  open Creds
  open Http_method

  module Util = Aws_util

  exception Error of string

  let sprint = Printf.sprintf
  let print = Printf.printf
    
  let signed_request 
      ?region
      ?(http_method=`POST) 
      ?(http_uri="/")
      ?expires_minutes
      creds 
      params = 
    
    let http_host =
      match region with
        | Some r -> sprint "sdb.%s.amazonaws.com" r
        | None -> "sdb.amazonaws.com"
  in
    
    let params = 
      ("Version", "2009-04-15" ) ::
        ("SignatureVersion", "2") ::
        ("SignatureMethod", "HmacSHA1") ::
        ("AWSAccessKeyId", creds.aws_access_key_id) :: 
        params
    in
    
    let params = 
      match expires_minutes with
        | Some i -> ("Expires", Util.minutes_from_now i) :: params 
        | None -> ("Timestamp", Util.now_as_string ()) :: params
    in
    
    let signature = 
      let sorted_params = Util.sort_assoc_list params in
      let key_equals_value = Util.encode_key_equals_value sorted_params in
      let uri_query_component = String.concat "&" key_equals_value in
      let string_to_sign = String.concat "\n" [ 
        string_of_http_method http_method ;
        String.lowercase http_host ;
        http_uri ;
        uri_query_component 
      ]
      in 
      let hmac_sha1_encoder = Cryptokit.MAC.hmac_sha1 creds.aws_secret_access_key in
      let signed_string = Cryptokit.hash_string hmac_sha1_encoder string_to_sign in
      Util.base64 signed_string 
    in
    
    let params = ("Signature", signature) :: params in
    (http_host ^ http_uri), params
      

  (* XML readers *)  
      
  let error_msg body =
    match X.xml_of_string body with
      | X.E ("Response",_,
             (X.E ("Errors",_,
                   [
                     X.E ("Error",_,[
                       X.E ("Code",_,[X.P code]);
                       X.E ("Message",_,[X.P message])
                     ])]))::_) -> `Error message
      | _ -> `Error "unknown message"


  let domain_of_xml = function 
    | X.E ("DomainName", _, [ X.P domain_name ]) -> domain_name 
    | _ -> raise (Error "ListDomainsResult.domain")

  let list_domains_response_of_xml = function 
    | X.E ("ListDomainsResponse", _, 
           [ 
             X.E ("ListDomainsResult", _, domains); 
             _ ]) -> List.map domain_of_xml domains
    | _ -> raise (Error "ListDomainsResult")
    

  (* list all domains *)
      
  let list_domains creds ?token () = 
    let url, params = signed_request creds 
      (("Action", "ListDomains")
       :: match token with 
           None -> []
         | Some t -> [ "NextToken", t ]) in
    
    try_lwt 
  lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
  print_endline body ;
  let xml = X.xml_of_string body in
  return (`Ok (list_domains_response_of_xml xml))
  with HC.Http_error (_, _, body) -> print_endline body ; return (error_msg body)



end
