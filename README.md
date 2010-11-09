Package to provide OCaml client access to Amazon services, such as S3,
EC2, FPS, etc.

Aws depends on the following packages:

-  omake
-  netstring
-  cryptokit
-  calendar
-  lwt
-  xml-light
-  cohttp: forked from to [avsm's
     cohttp](http://github.com/avs/ocaml-cohttp) to [barko's
     cohttp](http://github.com/barko/ocaml-cohttp)

All of the packages except the last are available via GODI.  

In an ideal world, the code in these modules would be generated
automatically, from a formal type definition.  The monstrosity know as
SOAP is painfully distant from this ideal.

