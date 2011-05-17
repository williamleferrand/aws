type message = {
  message_id : string;
  receipt_handle : string;
  body : string;
}

val create_queue :
  ?default_visibility_timeout:int ->
  Creds.t -> string -> 
  [> `Error of string | `Ok of string ] Lwt.t

val list_queues :
  ?prefix:string ->
  Creds.t -> 
  [> `Error of string | `Ok of string list ] Lwt.t

val receive_message :
  ?attribute_name:string ->
  ?max_number_of_messages:int ->
  ?visibility_timeout:int ->
  ?encoded:bool ->
  Creds.t -> string -> 
  [> `Error of string | `Ok of message list ] Lwt.t

val delete_message :
  Creds.t -> string -> string -> 
  [> `Error of string | `Ok of unit ] Lwt.t
