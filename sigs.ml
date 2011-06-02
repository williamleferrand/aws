(* Various signatures *)

module type HTTP_CLIENT = 
  sig
    type headers = (string * string) list
    type request_body =  [ `InChannel of int * Lwt_io.input_channel
                         | `None
                         | `String of string ]
        
    exception Http_error of (int * headers * string)

    val get : ?headers:headers -> string -> (headers * string) Lwt.t  
    
    val get_to_chan : ?headers:headers -> string -> Lwt_io.output_channel -> headers Lwt.t

    val post : ?headers:headers -> ?body:request_body -> string -> (headers * string) Lwt.t  

    val put : ?headers:headers -> ?body:request_body -> string -> (headers * string) Lwt.t  

    val delete : ?headers:headers -> string -> (headers * string) Lwt.t  

    val head  : ?headers:headers -> string -> (headers * string) Lwt.t  
  end
