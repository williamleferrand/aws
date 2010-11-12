Package to provide OCaml client access to Amazon services, such as S3,
EC2, FPS, etc.

Aws depends on the following packages:

-  omake
-  pcre
-  netstring
-  cryptokit
-  calendar
-  lwt
-  xml-light
-  [cohttp](http://github.com/avsm/ocaml-cohttp)

All of the packages except the last are available via GODI.  

In an ideal world, the code in these modules would be generated
automatically, from a formal type definition.  The monstrosity know as
SOAP is painfully distant from this ideal, which is why we don't use
it here.  For a hint at service API's could and ought to work, check
out: [atdgen](http://oss.wink.com/atdgen/),
[biniou](http://martin.jambon.free.fr/biniou.html), and [yojson](
http://martin.jambon.free.fr/yojson.html).