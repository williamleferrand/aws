type http_method = [`GET | `PUT | `POST | `HEAD | `DELETE ]

let string_of_http_method = function
  | `GET -> "GET"
  | `PUT -> "PUT"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `POST -> "POST"
