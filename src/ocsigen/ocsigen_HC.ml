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

let get_to_chan = failwith "get_to_chan"

let post ?(headers=[]) ?(body=`None) url =
  let url = Neturl.parse_url url in
  let host = Neturl.url_host url in
  let uri = Neturl.url_fragment url in
  let content = match body with
    | `None -> Lwt.return ""
    | `String s -> Lwt.return s
    | `InChannel (_,chan) -> Aws_util.read_contents chan
  in
  let content_type =
    let ct = List.assoc "Content-Type" headers in
    Aws_util.split_slash ct in
  let headers = List.fold_left
    (fun h (n,v) ->
      Http_headers.add (Http_headers.name n) v h)
    Http_headers.empty headers
  in
  content >>= fun content ->
  Ocsigen_http_client.post_string ~headers ~host ~uri ~content ~content_type () >>= fun frame ->
  match frame.frame_content with
      None -> return ([], "")
    | Some content -> let st = Ocsigen_stream.get content in
                      Ocsigen_stream.string_of_stream 15000 st >>= fun s -> return ([], s)

let put = failwith "put"
let delete = failwith "delete"
let head = failwith "head"

