type node = 
  | Element of string * attr list * node list 
  | PCData of string
and attr = (string * string) * string

val xml_of_string : string -> node
val string_of_xml : node -> string

val parse_string : string -> node
