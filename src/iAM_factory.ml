module Make = functor (HC : Aws_sigs.HTTP_CLIENT) ->
struct

open Lwt

module Util = Aws_util

module C = CalendarLib.Calendar
module P = CalendarLib.Printer.CalendarPrinter

exception Service_down

(* Miscellaneous *****************************************************************)

let remove_newline =
  Pcre.replace ~rex:(Pcre.regexp "\n") ~templ:""

let base64 str =
  (* the encoder is consumed by its use, so we have to recreated *)
  let b64_encoder = Cryptokit.Base64.encode_multiline () in
  let encoded = Cryptokit.transform_string b64_encoder str in

  (* we want to retain the trailing '=' characters, but eliminate the
     newlines.  Unfortunately, [encode_compact] has neither. *)
  remove_newline encoded

let encode_url ?(safe=false) str =
(*  if not safe then
    Netencoding.Url.encode ~plus:false str
  else *)
    begin
      let strlist = ref [] in
      let code = Char.code in
      for i = 0 to String.length str - 1 do
        let c = code (str.[i]) in
        if
          (65 <= c && c <= 90)
          || (48 <= c && c <= 57 )
          || (97 <= c && c <= 122)
          || (c = 126)
          || (c = 95)
          || (c = 46)
          || (c = 45)
          (* || (c = 47) *)
        then
	  strlist := Printf.sprintf "%c" str.[i] :: !strlist
	else if (c = 32)
	then
	  strlist := Printf.sprintf "%%20" :: !strlist
        else
	  strlist :=  Printf.sprintf "%%%X" c :: !strlist
      done ;
      String.concat "" (List.rev !strlist)
    end

let encode_key_equal_value ?(safe=false) (k,v) =
  (encode_url ~safe k) ^ "=" ^ (encode_url ~safe v)

let encode_key_equals_value ?safe kvs =
  List.map (encode_key_equal_value ?safe) kvs

let sort_assoc_list kv_list =
  List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) kv_list

let host = "sts.amazonaws.com"

let sign_request aws_access_key_id aws_secret_access_key params =
  let signature =
    let sorted_params = sort_assoc_list params in
    let key_equals_value = encode_key_equals_value sorted_params in
    let uri_query_component = String.concat "&" key_equals_value in
    let string_to_sign = String.concat "\n" [
      "POST" ;
      String.lowercase host ;
      "/" ;
      uri_query_component
    ]
    in
    let hmac_sha256_encoder = Cryptokit.MAC.hmac_sha256 aws_secret_access_key in
    let signed_string = Cryptokit.hash_string hmac_sha256_encoder string_to_sign in
    let signed_string = String.sub signed_string 0 ((String.length signed_string) - 0) in
    let b64 = base64 signed_string in
    String.sub b64 0 ((String.length b64) - 0)
  in
  ("Signature", signature)::params


type node =
  | E of string * attr list * node list
  | P of string
and attr = (string * string) * string

let xml_of_string s =
  (* we drop the namespace part of the element here *)
  let el ((ns, name), atts) kids = E (name, atts, kids) in
  let data d = P d in
  let input = Xmlm.make_input ~strip:true (`String (0,s)) in
  let _, node = Xmlm.input_doc_tree ~el ~data input in
  node

exception Invalid_credential

let xml_get_content_of name xml : string option =
  let rec aux = function
    | E (n,_,[P v]) when n=name -> Some v
    | E (_,_,l) ->
      let rec loop = function
        | [] -> None
	| x::xs -> match aux x with
	    | Some x -> Some x
	    | None -> loop xs
      in loop l
    | _ -> None
  in aux xml

type session_token = {
  session_token : string;
  secret_access_key : string;
  access_key_id : string;
  expiration : int64;
}

let token_is_valid token = token.expiration > (Int64.of_float (C.to_unixfloat (C.now ())))

let get_session_token ?duration ?(version="2011-06-15") aws_access_key_id aws_secret_access_key =
  let content =
    let s (n,v)= Some (n,v) in
    let n (n,v)= Util.option_map (fun v -> (n,string_of_int v)) v in
    let now = P.sprint "%FT%T" (C.from_unixfloat (Unix.gettimeofday ())) in
    s("AWSAccessKeyId",aws_access_key_id)
    ::s("Action","GetSessionToken")
    ::n("DurationSeconds",duration)
    ::s("SignatureMethod","HmacSHA256")
    ::s("SignatureVersion","2")
    ::s("Timestamp",now )
    ::s("Version",version)
    ::[] in
  let params = Util.filter_map (fun x -> x) content in
  let params = sign_request aws_access_key_id aws_secret_access_key params in
  let key_equals_value = encode_key_equals_value params in
  let content = String.concat "&" key_equals_value in
  lwt _,s = HC.post ~headers:["Content-Type","application/x-www-form-urlencoded"] ~body:(`String content) (Printf.sprintf "https://%s/" host) in
  let xml = xml_of_string s in
  let session_token = xml_get_content_of "SessionToken" xml
  and secret_access_key = xml_get_content_of "SecretAccessKey" xml
  and access_key_id = xml_get_content_of "AccessKeyId" xml
  and expiration = xml_get_content_of "Expiration" xml
  in match session_token,secret_access_key,access_key_id,expiration with
    | Some session_token,Some secret_access_key,Some access_key_id,Some expiration ->
      let expiration = Scanf.sscanf expiration "%d-%d-%dT%d:%d:%d.%dZ" Util.make in
      return {session_token; secret_access_key; access_key_id; expiration}
    | _ -> raise Invalid_credential
end
