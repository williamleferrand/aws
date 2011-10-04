module Util = Aws_util
module X = Xml
module HC = Http_client10

open Creds
open Printf
open Lwt

let cbui_http_uri = "/cobranded-ui/actions/start"
let fps_http_uri = "/"

let sandbox_cbui_host = "authorize.payments-sandbox.amazon.com"
let production_cbui_host = "authorize.payments.amazon.com"

let sandbox_fps_host = "fps.sandbox.amazonaws.com"
let production_fps_host = "fps.amazonaws.com"

let cbui_signature_params = [
  "signatureMethod", "HmacSHA256";
  "signatureVersion", "2";
  "version", "2009-01-09"
]

(* note capitalization of keys, different than [cbui_signature_params]
   !!! *)
let fps_signature_params = [
  "SignatureMethod", "HmacSHA256";
  "SignatureVersion", "2";
  "Version", "2010-08-28" 
]  

type cobranding_style = [`banner | `logo]

let string_of_cobranding_style = function
  | `banner -> "banner"
  | `logo   -> "logo"

type pipeline = [
| `SingleUse 
| `MultiUse 
| `Recurring 
| `Recipient 
| `SetupPrepaid 
| `SetupPostpaid 
| `EditToken
]

let string_of_pipeline = function
  | `SingleUse      -> "SingleUse"
  | `MultiUse       -> "MultiUse"    
  | `Recurring      -> "Recurring"
  | `Recipient      -> "Recipient"
  | `SetupPrepaid   -> "SetupPrepaid"
  | `SetupPostpaid  -> "SetupPostpaid"
  | `EditToken      -> "EditToken" 

type service_kind = [`FPS | `CBUI]

type scheme = [`HTTP | `HTTPS]
let string_of_scheme = function
  | `HTTP -> "http"
  | `HTTPS -> "https"

type url = {
  scheme : scheme;
  host : string;
  port : int option;
  query : string;
  params : (string * string) list
}

let string_of_url u =
  let buf = Buffer.create 100 in
  let add = Buffer.add_string buf in
  add (string_of_scheme u.scheme);
  add "://";
  add u.host;
  (match u.port with
     | Some p -> add (":" ^ (string_of_int p))
     | None -> ()
  );
  add u.query; (* remember the leading '/' ! *)
  (match u.params with
     | _ :: _  ->
         add "?";
         let key_equals_value = Util.encode_key_equals_value u.params in
         let params_sep = String.concat "&" key_equals_value in
         add params_sep
     | [] -> ()
  );
  Buffer.contents buf

let modify_url_endpoint u alt_scheme alt_host alt_port =
  let u = 
    match alt_host with
      | Some host -> { u with host }
      | None -> u
  in
  let u = 
    match alt_port with
      | Some port -> { u with port = (Some port) }
      | None -> u
  in
  let u = 
    match alt_scheme with
      | Some scheme -> { u with scheme }
      | None -> u
  in
  u

(* sign a request, and produce a url *)
let sign_request creds ?(sandbox=false) http_method params service_kind = 
  let http_host, http_uri =
    match sandbox, service_kind with
      | true , `CBUI -> sandbox_cbui_host   , cbui_http_uri
      | false, `CBUI -> production_cbui_host, cbui_http_uri
      | true , `FPS  -> sandbox_fps_host    , fps_http_uri
      | false, `FPS  -> production_fps_host , fps_http_uri
  in

  let signature = 
    let sorted_params = Util.sort_assoc_list params in
    let key_equals_value = Util.encode_key_equals_value sorted_params in
    let uri_query_component = String.concat "&" key_equals_value in
    let string_to_sign = String.concat "\n" [ 
      Http_method.string_of_t http_method ;
      String.lowercase http_host ;
      http_uri ;
      uri_query_component 
    ]
    in
    let hmac_sha256_encoder = Cryptokit.MAC.hmac_sha256 creds.aws_secret_access_key in
    let signed_string = Cryptokit.hash_string hmac_sha256_encoder string_to_sign in
    Util.base64 signed_string 
  in

  (* signature *)
  let signature_key =
    match service_kind with
      | `CBUI -> "signature"
      | `FPS  -> "Signature" (* shamefull, Amazon! *)
  in
  let params = (signature_key, signature) :: params in
  {
    scheme = `HTTPS;
    port = None;
    host = http_host;
    query = http_uri;
    params;
  }


type payment_method = [ `CC | `ACH | `ABT ]

let string_of_payment_method = function
  | `CC  -> "CC"
  | `ACH -> "ACH"
  | `ABT -> "ABT"


let cap_string_of_bool = function
  | true -> "True"
  | false -> "False"

let add_opt params o f =
  match o with 
    | Some v -> (f v) :: params 
    | None -> params

type signature_info = {
  certificate_url : string;
  signature : string;
  signature_version : int;
  signature_method : string;
}

module SingleUse = 
struct

  module CBUI = struct 
    module Request = struct
      type t = {
        address_line_1 : string option;
        address_line_2 : string option;
        address_name : string option;
        caller_reference : string;
        city : string option;
        cobranding_style : cobranding_style option;
        cobranding_url : string option;
        collect_shipping_address : bool option;
        country : string option;
        currency_code : string option  (* TODO: variant *);
        discount : float option;
        gift_wrapping : float option;
        handling : float option;
        item_total : float option;
        payment_methods : payment_method list option;
        payment_reason : string option;
        reserve : bool option;
        return_url : string;
        shipping : float option;
        state : string option;
        tax : float option;
        transaction_amount : float ;
        website_description : string option;
        zip : string option;
      }

      let create ~caller_reference ~return_url transaction_amount = {
        address_line_1 = None;
        address_line_2 = None;
        address_name = None;
        caller_reference = caller_reference;
        city = None;
        cobranding_style = None;
        cobranding_url = None;
        collect_shipping_address = None;
        country = None;
        currency_code = None;
        discount = None;
        gift_wrapping = None;
        handling = None;
        item_total = None;
        payment_methods = None;
        payment_reason = None;
        reserve = None;
        return_url = return_url;
        shipping = None;
        state = None;
        tax = None;
        transaction_amount = transaction_amount;
        website_description = None;
        zip = None;
      }

      let to_url ?(sandbox=false) creds t = 
        (* start with signature params *)
        let params = cbui_signature_params in

        (* address line 1 *)
        let params = add_opt params t.address_line_1 
          (fun v -> ("addressLine1", v)) in

        (* address line 2 *)
        let params = add_opt params t.address_line_2 
          (fun v -> ("addressLine2", v)) in

        (* address name *)
        let params = add_opt params t.address_name 
          (fun v -> ("addressName", v)) in

        (* caller reference *)
        let params = ("callerReference", t.caller_reference) :: params in

        (* city *)
        let params = add_opt params t.city
          (fun v -> ("city", v)) in
        
        (* cobranding style *)
        let params = add_opt params t.cobranding_style
          (fun v -> ("cobrandingStyle", string_of_cobranding_style v)) in

        (* cobranding image url *)
        let params = add_opt params t.cobranding_url 
          (fun v -> ("cobrandingUrl", v)) in

        (* country *)
        let params = add_opt params t.country
          (fun v -> ("country", v)) in

        (* collect shipping address *)
        let params = add_opt params t.collect_shipping_address 
          (fun v -> ("collectShippingAddress", cap_string_of_bool v)) in

        (* currency code *)
        let params = add_opt params t.currency_code
          (fun v -> ("currencyCode", v)) in

        (* discount *)
        let params = add_opt params t.discount
          (fun v -> ("discount", string_of_float v)) in

        (* gift wrapping *)
        let params = add_opt params t.gift_wrapping 
          (fun v -> ("giftWrapping", string_of_float v)) in

        (* handling *)
        let params = add_opt params t.handling
          (fun v -> ("handling", string_of_float v)) in

        (* item total *)
        let params = add_opt params t.item_total
          (fun v -> ("itemTotal", string_of_float v)) in

        (* payment methods *)
        let params = add_opt params t.payment_methods
          (fun v ->
             (* comma-separate *)
             (* TODO: check that each appears only once *)
             let payment_methods_s = 
               String.concat "," (List.map string_of_payment_method v) in
             ("paymentMethod", payment_methods_s)
          ) in

        (* payment reason *)
        let params = add_opt params t.payment_reason 
          (fun v -> ("paymentReason", v)) in

        (* reserve *)
        let params = add_opt params t.reserve
          (fun v -> ("reserve", cap_string_of_bool v)) in

        (* return URL *)
        let params = ("returnURL", t.return_url) :: params in

        (* shipping *)
        let params = add_opt params t.shipping
          (fun v -> ("shipping", string_of_float v)) in

        (* state *)
        let params = add_opt params t.state
          (fun v -> ("state", v)) in

        (* tax *)
        let params = add_opt params t.tax
          (fun v -> ("tax", string_of_float v)) in

        (* transaction amount *)
        let params = ("transactionAmount", string_of_float t.transaction_amount) :: params in

        (* website description *)
        let params = add_opt params t.website_description 
          (fun v -> ("websiteDescription", v)) in

        (* zip *)
        let params = add_opt params t.zip
          (fun v -> ("zip", v)) in

        (* pipeline *)
        let params = ("pipelineName", string_of_pipeline `SingleUse) :: params in

        (* aws access key id of the caller *)
        let params = ("callerKey", creds.aws_access_key_id) :: params in

        let u = sign_request creds ~sandbox `GET params `CBUI in
        string_of_url u


    end

    module Response =
    struct

      type tok = {
        address_line_1 : string option;
        address_line_2 : string option;
        address_name : string option;
        city : string option;
        state : string option;
        zip : string option;
        phone_number : string option;
        expiry : string option;
        token_id : string;
      }      

      type t = {
        signature_info : signature_info;
        params : (string * string) list;
        result : [ `Bad of string | `Token of tok ]
      }

      let signature_info params =
        let find x = List.assoc x params in
        try
          Some {
            certificate_url = find "certificateUrl";
            signature = find "signature";
            signature_version = int_of_string (find "signatureVersion");
            signature_method = find "signatureMethod"
          }
        with 
          | Not_found -> None (* some required parameter is not found *)
          | Failure _ -> None (* int_of_string failed *)


      let of_url uri =
        let params = Aws_util.url_params uri in
        match signature_info params with
          | None -> None
          | Some signature_info ->
              let find_opt x = try Some (List.assoc x params) with Not_found -> None in
              match find_opt "errorMessage" with
                | Some msg ->
                    Some { signature_info; params; result = `Bad msg }
                | None -> (

                    match find_opt "tokenID" with
                      | None -> None
                      | Some token_id ->
                          let tok = {
                            token_id;
                            address_line_1 = find_opt "addressLine1";
                            address_line_2 = find_opt "addressLine2";
                            address_name = find_opt "addressName";
                            city = find_opt "city";
                            state = find_opt "state";
                            zip = find_opt "zip";
                            phone_number = find_opt "phoneNumber";
                            expiry = find_opt "expiry";
                          } in
                          Some { signature_info; params; result = `Token tok }
                  )
    end
  end

  module Pay = struct

    type customer_service_owner = [ `Caller | `Recipient ]
    type soft_descriptor_type = [ `Static | `Dynamic of string ] 
        (* [`Dynamic] -> SenderDescription *)
        
    type descriptor_policy = {
      customer_service_owner : customer_service_owner ;
      soft_descriptor_type : soft_descriptor_type 
    }

    let string_of_customer_service_owner = function
      | `Caller -> "Caller"
      | `Recipient -> "Recipient"

    let string_of_soft_descriptor_type = function
      | `Static -> "Static"
      | `Dynamic _ -> "Dynamic"

    let params_of_descriptor_policy dp =
      let params =
        match dp.soft_descriptor_type with
          | `Dynamic sender_description ->
              ["SenderDescription", sender_description]
          | `Static -> []
      in

      ["DescriptorPolicy.CSOwner", 
       string_of_customer_service_owner dp.customer_service_owner;
       "DescriptorPolicy.SoftDescriptorType", 
       string_of_soft_descriptor_type dp.soft_descriptor_type
      ] @ params

    type t = {
      caller_description : string option;
      caller_reference : string;
      descriptor_policy : descriptor_policy option;
      sender_token_id : string;
      transaction_amount : float;
      currency_code : string;
      transaction_timeout_minutes : int option;
      expires_minutes : int option
    }

    let create ~sender_token_id ~transaction_amount ~caller_reference = {
      caller_description = None;
      caller_reference;
      descriptor_policy = None;
      sender_token_id;
      transaction_amount;
      currency_code = "USD";
      transaction_timeout_minutes = None;
      expires_minutes = None;
    }

    let to_url creds ?(sandbox=false) t =
      let params = fps_signature_params in
      let params = 
        match t.descriptor_policy with
          | None -> params 
          | Some dp -> (params_of_descriptor_policy dp) @ params
      in

      (* Timestamp or Expires *)
      let toe = 
        match t.expires_minutes with
          | Some i -> ("Expires", Util.minutes_from_now i)
          | None -> ("Timestamp", Util.now_as_string ())
      in

      let params = 
        ("AWSAccessKeyId", creds.aws_access_key_id) ::
          ("Action", "Pay") :: 
          toe :: 
          ("CallerReference", t.caller_reference) ::
          ("SenderTokenId", t.sender_token_id) ::
          ("TransactionAmount.Value", string_of_float t.transaction_amount) ::
          ("TransactionAmount.CurrencyCode", t.currency_code ) :: params in

      let params = add_opt params t.transaction_timeout_minutes 
        (fun v -> "TransactionTimeoutInMins", string_of_int v) in

      sign_request creds ~sandbox `GET params `FPS


    type transaction_status = [ `Cancelled | `Failure | `Pending | `Reserved | `Success ]
        
    let transaction_status_of_string = function
      | "Cancelled"  -> Some `Cancelled 
      | "Failure"    -> Some `Failure 
      | "Pending"    -> Some `Pending 
      | "Reserved"   -> Some `Reserved 
      | "Success"    -> Some `Success    
      | _            -> None


    type error = [
    | `AccessFailure
    | `AccountLimitsExceeded
    | `AmountOutOfRange
    | `AuthFailure 
    | `DuplicateRequest 
    | `IncompatibleTokens
    | `InsufficientBalance
    | `InternalError
    | `InvalidAccountState_Caller
    | `InvalidAccountState_Recipient
    | `InvalidAccountState_Sender
    | `InvalidClientTokenId 
    | `InvalidParams 
    | `InvalidTokenId_Sender
    | `SameSenderAndRecipient
    | `SignatureDoesNotMatch 
    | `TokenNotActive_Sender
    | `TransactionDenied
    | `UnverifiedAccount_Recipient
    | `UnverifiedAccount_Sender
    | `UnverifiedBankAccount
    | `UnverifiedEmailAddress_Caller
    | `UnverifiedEmailAddress_Recipient
    | `UnverifiedEmailAddress_Sender            
    ]

    let string_of_error = function
    | `AccessFailure                    -> "AccessFailure"                     
    | `AccountLimitsExceeded            -> "AccountLimitsExceeded"             
    | `AmountOutOfRange                 -> "AmountOutOfRange"                  
    | `AuthFailure                      -> "AuthFailure"                       
    | `DuplicateRequest                 -> "DuplicateRequest"                  
    | `IncompatibleTokens               -> "IncompatibleTokens"                
    | `InsufficientBalance              -> "InsufficientBalance"               
    | `InternalError                    -> "InternalError"                     
    | `InvalidAccountState_Caller       -> "InvalidAccountState_Caller"        
    | `InvalidAccountState_Recipient    -> "InvalidAccountState_Recipient"     
    | `InvalidAccountState_Sender       -> "InvalidAccountState_Sender"        
    | `InvalidClientTokenId             -> "InvalidClientTokenId"              
    | `InvalidParams                    -> "InvalidParams"                     
    | `InvalidTokenId_Sender            -> "InvalidTokenId_Sender"             
    | `SameSenderAndRecipient           -> "SameSenderAndRecipient"            
    | `SignatureDoesNotMatch            -> "SignatureDoesNotMatch"             
    | `TokenNotActive_Sender            -> "TokenNotActive_Sender"             
    | `TransactionDenied                -> "TransactionDenied"                 
    | `UnverifiedAccount_Recipient      -> "UnverifiedAccount_Recipient"       
    | `UnverifiedAccount_Sender         -> "UnverifiedAccount_Sender"          
    | `UnverifiedBankAccount            -> "UnverifiedBankAccount"             
    | `UnverifiedEmailAddress_Caller    -> "UnverifiedEmailAddress_Caller"     
    | `UnverifiedEmailAddress_Recipient -> "UnverifiedEmailAddress_Recipient"  
    | `UnverifiedEmailAddress_Sender    -> "UnverifiedEmailAddress_Sender"           


    let error_of_string = function
      | "AccessFailure"                     -> Some `AccessFailure                       
      | "AccountLimitsExceeded"             -> Some `AccountLimitsExceeded               
      | "AmountOutOfRange"                  -> Some `AmountOutOfRange                    
      | "AuthFailure"                       -> Some `AuthFailure 
      | "DuplicateRequest"                  -> Some `DuplicateRequest 
      | "IncompatibleTokens"                -> Some `IncompatibleTokens                  
      | "InsufficientBalance"               -> Some `InsufficientBalance                 
      | "InternalError"                     -> Some `InternalError                       
      | "InvalidAccountState_Caller"        -> Some `InvalidAccountState_Caller          
      | "InvalidAccountState_Recipient"     -> Some `InvalidAccountState_Recipient       
      | "InvalidAccountState_Sender"        -> Some `InvalidAccountState_Sender          
      | "InvalidClientTokenId"              -> Some `InvalidClientTokenId 
      | "InvalidParams"                     -> Some `InvalidParams 
      | "InvalidTokenId_Sender"             -> Some `InvalidTokenId_Sender               
      | "SameSenderAndRecipient"            -> Some `SameSenderAndRecipient              
      | "SignatureDoesNotMatch"             -> Some `SignatureDoesNotMatch 
      | "TokenNotActive_Sender"             -> Some `TokenNotActive_Sender               
      | "TransactionDenied"                 -> Some `TransactionDenied                   
      | "UnverifiedAccount_Recipient"       -> Some `UnverifiedAccount_Recipient         
      | "UnverifiedAccount_Sender"          -> Some `UnverifiedAccount_Sender            
      | "UnverifiedBankAccount"             -> Some `UnverifiedBankAccount               
      | "UnverifiedEmailAddress_Caller"     -> Some `UnverifiedEmailAddress_Caller       
      | "UnverifiedEmailAddress_Recipient"  -> Some `UnverifiedEmailAddress_Recipient    
      | "UnverifiedEmailAddress_Sender"     -> Some `UnverifiedEmailAddress_Sender       
      | _                                   -> None

    let is_error_fatal = function
      | `AuthFailure 
      | `DuplicateRequest 
      | `InvalidClientTokenId 
      | `InvalidParams 
      | `SignatureDoesNotMatch -> true
      | _ -> false


    (* bad outcome *)
    let error_of_xml = function
      | X.E("Error", _, kids) -> (
          match kids with
            | X.E("Code", _, [X.P code] ) :: _ -> (
                match error_of_string code with
                  | Some err -> Some err
                  | None -> None
              )
            | _ -> None
        )
      | _ -> None

    let errors_of_xml s kids =
      let have_parse_error, errors = List.fold_left (
        fun (have_parse_error, errors) node ->
          match error_of_xml node with
            | None -> true, errors
            | Some e -> have_parse_error, e :: errors
      ) (false, []) kids in
      if have_parse_error then
        `Error s
      else
        `Bad errors

    let response_of_xml s = function
      | X.E("Errors", _, kids ) :: _ ->
          errors_of_xml s kids
      | _ -> `Error s

    (* good outcome *)
    let pay_response_of_xml s = function
      | X.E("PayResult", _, kids ) :: _ -> (
          match kids with
            | [X.E("TransactionId", _, [X.P id] );
               X.E("TransactionStatus", _, [X.P status] ) ] -> (
                match transaction_status_of_string status with
                  | None -> `Error s
                  | Some transaction_status -> 
                      `Ok (id, transaction_status)
              )
            | _ -> `Error s
        )
      | _ -> `Error s

    let of_xml s =
      match X.xml_of_string s with
        | X.E ("PayResponse", _, kids) ->
            pay_response_of_xml s kids

        | X.E ("Response", _, kids ) ->
            response_of_xml s kids

        | _ -> `Error s


    let call creds ?alt_scheme ?alt_host ?alt_port ?(sandbox=false) t =
      let u = to_url creds ~sandbox t in
      (* make sure the http client uses the ultimate target host as the value of
         the ["Host"] header, when [alt_host] is provided *)
      let headers =
        match alt_host with
          | Some _ -> ["Host", u.host ]
          | None -> []
      in

      let u = modify_url_endpoint u alt_scheme alt_host alt_port in
      let u_s = string_of_url u in

      try_lwt
        lwt _, body = HC.get ~headers u_s in
        return (of_xml body)
      with HC.Http_error (_,_,msg) -> 
        (* error condition with a http return code <> 200? perhaps *)
        return (of_xml msg)

  end

end
  
module VerifySignature =
struct 

  type error = [
    | `InvalidParams of string
    | `InternalServerError 
  ]

  let string_of_error = function
    | `InvalidParams msg -> sprintf "(InvalidParams %s)" msg
    | `InternalServerError -> "InternalServerError"

  let error_of_xml s = function
    | X.E ("Code", _, [X.P "InvalidParams"]) :: X.E ("Message", _, [X.P message]) :: _ ->
        `Bad (`InvalidParams message)
    | X.E ("Code", _, [X.P "InternalServerError"]) :: _ ->
        `Bad `InternalServerError
    | _ -> `Error s

  let errors_of_xml s = function
    | X.E ("Error", _, kids ) :: _ ->
        error_of_xml s kids
    | _ -> `Error s

  let response_of_xml s = function
    | X.E ("Errors", _, kids ) :: _ ->
        errors_of_xml s kids
    | _ -> `Error s


  (* good *)
  let verify_signature_response_of_xml s = function
    | X.E ("VerifySignatureResult", _, kids) :: _  -> (
        match kids with
          | X.E ("VerificationStatus", _, [kid]) :: _ -> (
              match kid with
                | X.P "Success" -> `Success
                | X.P "Failure" -> `Failure
                | _ -> `Error s
            )
          | _ -> `Error s
      )
    | _ -> `Error s

  let of_xml s =
    match X.xml_of_string s with
      | X.E ("VerifySignatureResponse", _, kids) ->
          verify_signature_response_of_xml s kids

      | X.E ("Reponse", _, kids) ->
          response_of_xml s kids

      | _ -> `Error s

  let call creds ?alt_scheme ?alt_host ?alt_port ?(sandbox=false) url_endpoint http_params =
    let http_params = String.concat "&" (Aws_util.encode_key_equals_value http_params) in
    let params = [
      "Timestamp", Util.now_as_string ();
      "AWSAccessKeyId", creds.aws_access_key_id;
      "HttpParameters", http_params;
      "UrlEndPoint", url_endpoint;
      "Action", "VerifySignature"
    ] @ fps_signature_params in
    let u = sign_request creds ~sandbox `GET params `FPS in
    let u' = modify_url_endpoint u alt_scheme alt_host alt_port in
    let u_s = string_of_url u' in

    let headers =
      match alt_host with
        | Some _ -> ["Host", u.host ]
        | None -> []
    in

    try_lwt
      lwt _, body = HC.get ~headers u_s in
      return (of_xml body)
    with HC.Http_error (_,_,msg) -> 
      return (`Error msg)
      
end

