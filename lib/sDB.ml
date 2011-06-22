module Http_conf : Cohttp.Http_client_pool.Http_conf =
  struct
    let nb_socket = 10
    let host = "sdb.amazonaws.com"
    let port = 80

end

module Http_pool = Cohttp.Http_client_pool.Make (Http_conf)

module M = SDB_factory.Make (Http_pool)

include M
