type node = 
  | Element of string * attr list * node list 
  | PCData of string
and attr = (string * string) * string

let xml_of_string s =
  (* we drop the namespace part of the element here *)
  let el ((ns, name), atts) kids = Element (name, atts, kids) in
  let data d = PCData d in
  let input = Xmlm.make_input ~strip:true (`String (0,s)) in
  let _, node = Xmlm.input_doc_tree ~el ~data input in
  node
  
let parse_string = xml_of_string

let frag = function
  | Element (name, attrs, kids) -> `El ((("", name), attrs), kids) 
  | PCData d -> `Data d 

let string_of_xml x =
  let buf = Buffer.create 100 in
  let output = Xmlm.make_output (`Buffer buf) in
  Xmlm.output_doc_tree frag output (None, x);
  Buffer.contents buf
  
