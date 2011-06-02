(* Various signatures *)

module type HTTP_CLIENT = 
  sig
    type headers
    
    exception Http_error of (int * headers * string)

    val get : ?headers:headers -> string -> (headers * string) Lwt.t  
  end
