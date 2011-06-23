

module Cc = 
struct
  let params = Hashtbl.create 0 
    
  let get_param = Hashtbl.find params
  let set_param = Hashtbl.replace params
    
  let _ = 
    set_param "http_pool_size" "5"
end


module H = Http_pool.Make (Cc)

module H2 = 
  struct 
    include Http_client
    include H
  end

module M = SDB_factory.Make (H2)
include M

