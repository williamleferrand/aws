module M = SQS_functor.Make (Cohttp.Http_client)

include M
