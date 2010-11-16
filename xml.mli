type node = 
  | E of string * attr list * node list 
  | P of string
and attr = (string * string) * string

val xml_of_string : string -> node
val string_of_xml : node -> string

val parse_string : string -> node
