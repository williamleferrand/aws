type scheme = [`HTTP | `HTTPS]
    (* http scheme *)

type cobranding_style = [ `banner | `logo ]
type payment_method = [ `ABT | `ACH | `CC ]

type signature_info = {
  certificate_url : string;
  signature : string;
  signature_version : int;
  signature_method : string;
}

module SingleUse :
sig
  module CBUI :
  sig
    module Request :
    sig
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
        currency_code : string option;
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
        transaction_amount : float;
        website_description : string option;
        zip : string option;
      }
      val create :
        caller_reference:string -> return_url:string -> float -> t
      val to_url : ?sandbox:bool -> Creds.t -> t -> string
    end
    module Response :
    sig
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
        result : [ `Error of string | `Token of tok ];
      }
      val of_url : string -> t option
    end
  end

  module Pay :
  sig
    type customer_service_owner = [ `Caller | `Recipient ]
    type soft_descriptor_type = [ `Dynamic of string | `Static ]

    type descriptor_policy = {
      customer_service_owner : customer_service_owner;
      soft_descriptor_type : soft_descriptor_type;
    }

    type t = {
      caller_description : string option;
      caller_reference : string;
      descriptor_policy : descriptor_policy option;
      sender_token_id : string;
      transaction_amount : float;
      currency_code : string;
      transaction_timeout_minutes : int option;
      expires_minutes : int option;
    }

    type transaction_status =
        [ `Cancelled | `Failure | `Pending | `Reserved | `Success ]

    val create :
      sender_token_id:string ->
      transaction_amount:float -> caller_reference:string -> t

    (* [alt_{scheme,host,port}] used to make things work with stunnel *)
    val call : 
      Creds.t ->
      ?alt_scheme:scheme ->
      ?alt_host:string ->
      ?alt_port:int ->
      ?sandbox:bool ->
      t ->
      [ `Error of string | `Ok of string * transaction_status ] Lwt.t

  end

end


module VerifySignature : 
sig

  (* [alt_{scheme,host,port}] used to make things work with stunnel *)
  val call : Creds.t -> 
    ?alt_scheme:scheme -> 
    ?alt_host:string -> 
    ?alt_port:int -> 
    ?sandbox:bool -> string -> (string * string) list -> 
    [ `Error of string | `Failure | `Success ] Lwt.t

end

