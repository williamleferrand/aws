type t = [`GET | `PUT | `POST | `HEAD | `DELETE ]

let string_of_t = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `POST -> "POST"
