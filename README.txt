(* OASIS_START *)
(* DO NOT EDIT (digest: 3be1977e025b4011b6e0a6f491a10e79) *)
This is the README file for the aws distribution.

AWS client for Amazon Web Services

Package to provide OCaml client access to Amazon services : S3, EC2, SQS,
SDB, FPS, IAM, DynamoDb, SES.

Aws depends directly on the following packages:

 - netstring  - cryptokit  - calendar  - lwt  - xmlm  - yojson

It also depends on one of those packages:

  - [cohttp fork] (http://github.com/williamleferrand/ocaml-cohttp)
(--enable-cohttp)

  - ocsigen (--enable-ocsigen) (default)

the cohttp driver does not support ssl for now. Some services (e.g. Ses may
no be available when using cohttp)

All of the packages except the [cohttp fork] are available via OPAM.

See the files INSTALL.txt for building and installation instructions. 


(* OASIS_STOP *)
