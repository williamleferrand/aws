open Lwt

open Ocsigen_http_frame

type headers = (string * string) list
type request_body =  [ `InChannel of int * Lwt_io.input_channel
                     | `None
                     | `String of string ]
    
exception Http_error of (int * headers * string)
    
let get ?headers url =
  let url = Neturl.parse_url url in
  let host = Neturl.url_host url in
  let uri = Neturl.url_fragment url in
  Ocsigen_http_client.get ~host ~uri () 
  >>= fun frame -> 
  match frame.frame_content with 
      None -> return ([], "")
    | Some content -> let st = Ocsigen_stream.get content in
                      Ocsigen_stream.string_of_stream 15000 st >>= fun s -> return ([], s)

let get_to_chan = Obj.magic ()
let post = Obj.magic ()
let put = Obj.magic ()
let delete = Obj.magic ()
let head = Obj.magic ()

