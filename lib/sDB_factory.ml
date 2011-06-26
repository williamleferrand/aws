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
      ?(safe=false)
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
      let key_equals_value = Util.encode_key_equals_value ~safe sorted_params in
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
      
  let error_msg code' body =
    match X.xml_of_string body with
      | X.E ("Response",_,
             (X.E ("Errors",_,
                   [
                     X.E ("Error",_,[
                       X.E ("Code",_,[X.P code]);
                       X.E ("Message",_,[X.P message])
                     ])]))::_) -> `Error (code', message)
      | _ -> `Error (0, "unknown message")


  let domain_of_xml = function 
    | X.E ("DomainName", _, [ X.P domain_name ]) -> domain_name 
    | _ -> raise (Error "ListDomainsResult.domain")

  let list_domains_response_of_xml = function 
    | X.E ("ListDomainsResponse", _, 
           [ 
             X.E ("ListDomainsResult", _, domains); 
             _ ]) -> List.map domain_of_xml domains
    | _ -> raise (Error "ListDomainsResult")
    
  let attributes_of_xml encoded = function 
    | X.E ("Attribute", _, 
           [
             X.E ("Name", _, [ X.P name ]); 
             X.E ("Value", _, [ X.P value ]); 
           ]) -> ((if encoded then Util.base64_decoder name else name),  
                  (if encoded then Util.base64_decoder value else value))

    | _ -> raise (Error "Attribute")

  let get_attributes_response_of_xml encoded = function 
    | X.E ("GetAttributesResponse", _, 
           [ 
             X.E ("GetAttributesResult", _, attributes); 
             _; 
           ]) -> List.map (attributes_of_xml encoded) attributes
    | _ -> raise (Error "GetAttributesResponse") 


  let attrs_of_xml encoded = function 
    | X.E ("Attribute", _ , 
           [
             X.E ("Name", _, [ X.P name ]); 
             X.E ("Value", _, [ X.P value ])
           ]) -> (if encoded then Util.base64_decoder name else name), (if encoded then Util.base64_decoder value else value)  
    | _ -> raise (Error "Attribute")

  let attrs_of_xml encoded = function 
    | X.E ("Attribute", _ , 
           [
             X.E ("Name", _, [ X.P name ]) ;
             X.E ("Value", _, [ X.P value ]) ;
           ]) ->  (if encoded then Util.base64_decoder name else name), (Some  (if encoded then Util.base64_decoder value else value))
    | X.E ("Attribute", _ , 
           [
             X.E ("Name", _, [ X.P name ]) ;
             _
           ]) -> (if encoded then Util.base64_decoder name else name), None  
    | _ -> raise (Error "Attribute")

  let rec item_of_xml encoded acc token = function 
    | [] -> (acc, token)
    | X.E ("Item", _, (X.E ("Name", _, [ X.P name ]) :: attrs)) :: nxt -> item_of_xml encoded (((if encoded then Util.base64_decoder name else name), (List.map (attrs_of_xml encoded) attrs)) :: acc) token nxt
    | X.E ("NextToken", _, [ X.P next_token ]) :: _ -> acc, (Some next_token) 
    | _ -> raise (Error "Item")
      
  let select_of_xml encoded = function 
    | X.E ("SelectResponse", _,
           [
             X.E ("SelectResult", _, items); 
             _ ;
           ]) -> item_of_xml encoded [] None items
    | _ -> raise (Error "SelectResponse")

(* list all domains *)

  let list_domains creds ?token () = 
    let url, params = signed_request creds 
      (("Action", "ListDomains")
       :: match token with 
           None -> []
         | Some t -> [ "NextToken", t ]) in
    
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       let xml = X.xml_of_string body in
       return (`Ok (list_domains_response_of_xml xml))
    with HC.Http_error (code, _, body) ->  return (error_msg code body)

(* create domain *)

  let create_domain creds name = 
    let url, params = signed_request creds
      [
        "Action", "CreateDomain" ; 
        "DomainName", name
      ] in
    
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       return `Ok
    with HC.Http_error (code, _, body) ->  return (error_msg code body)

(* delete domain *)

  let delete_domain creds name = 
    let url, params = signed_request creds
      [
        "Action", "DeleteDomain" ; 
        "DomainName", name
      ] in
    
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       return `Ok
    with HC.Http_error (code, _, body) ->  return (error_msg code body)

(* put attributes *)
  
  let put_attributes ?(replace=false) ?(encode=true) creds domain item attrs = 
    let _, attrs' = 
      List.fold_left 
        (fun (i, acc) (name, value) -> 
          (i+1), ((sprint "Attribute.%d.Name" i, (if encode then Util.base64 name else name)) 
          :: (sprint "Attribute.%d.Value" i, (if encode then Util.base64 value else value))
          :: (if replace then 
               (sprint "Attribute.%d.Replace" i, "true") :: acc 
            else acc))) (1, []) attrs in
    let url, params = signed_request creds
      (("Action", "PutAttributes") 
       :: ("DomainName", domain)
       :: ("ItemName", (if encode then Util.base64 item else item))
       :: attrs') in 
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       return `Ok
    with HC.Http_error (code, _, body) -> return (error_msg code body)

(* batch put attributes *)
      
  let batch_put_attributes ?(replace=false) ?(encode=true) creds domain items =
    let _, attrs' = 
      List.fold_left 
        (fun (i, acc) (name, attrs) -> 
          let _, acc = (List.fold_left 
                          (fun (j, acc) (name, value) -> 
                            (j+1), ((sprint "Item.%d.Attribute.%d.Name" i j,  (if encode then Util.base64 name else name))
                                    :: (sprint "Item.%d.Attribute.%d.Value" i j, (if encode then Util.base64 value else value))
                                    :: (if replace then (sprint "Item.%d.Attribute.%d.Replace" i j, "true") :: acc else acc)
                                      
                             )) (1, (sprint "Item.%d.ItemName" i, (if encode then Util.base64 name else name)) :: acc) attrs) in 
        (i+1), acc)
        (1, []) items in 
        
    let url, params = signed_request creds
      (("Action", "BatchPutAttributes") 
       :: ("DomainName", domain)
       :: attrs') in 
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       return `Ok
    with HC.Http_error (code, _, body) ->  return (error_msg code body)
    

    
(* get attributes *)

  let get_attributes ?(encoded=true) creds domain ?attribute item = 
    let url, params = signed_request creds
      (("Action", "GetAttributes") 
       :: ("DomainName", domain)
       :: ("ItemName", (if encoded then Util.base64 item else item))
       :: (match attribute with 
         | None -> [] 
         | Some attribute_name -> [ "AttributeName", (if encoded then Util.base64 attribute_name else attribute_name) ])) in 
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       let xml = X.xml_of_string body in
       return (`Ok (get_attributes_response_of_xml encoded xml))
    with HC.Http_error (code, _, body) ->  return (error_msg code body)
 
(* delete attributes *)

  let delete_attributes ?(encode=true) creds domain item attrs = 
    let attrs' = 
      List.fold_left 
        (fun acc (i, name, value) -> 
          (sprint "Attribute.%d.Name" i, (if encode then Util.base64 name else name)) 
          :: (sprint "Attribute.%d.Value" i, (if encode then Util.base64 value else value))
          :: acc) [] attrs in
    let url, params = signed_request creds
      (("Action", "DeleteAttributes") 
       :: ("DomainName", domain)
       :: ("ItemName", (if encode then Util.base64 item else item))
       :: attrs') in 
    try_lwt 
       lwt header, body = HC.post ~body:(`String (Util.encode_post_url params)) url in
       
       return `Ok
    with HC.Http_error (code, _, body) ->  return (error_msg code body)
 
(* select *)

  let select ?(consistent=false) ?(encoded=true) ?(token=None) creds expression =
    let url, params = signed_request ~safe:true creds
      (("Action", "Select") 
       :: ("SelectExpression", expression)
       :: ("ConsistentRead", sprint "%B" consistent)
       :: (match token with 
         | None -> []
         | Some t -> [ "NextToken", t ])) in 
    try_lwt 
  let key_equals_value = Util.encode_key_equals_value ~safe:true params in
  let uri_query_component = String.concat "&" key_equals_value in
       lwt header, body = HC.post ~body:(`String uri_query_component) url in
       
       let xml = X.xml_of_string body in
       return (`Ok (select_of_xml encoded xml))
    with HC.Http_error (code, _, body) -> return (error_msg code body)
 
end
