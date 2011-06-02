# Aws Makefile

# Cohttp edition

all: ocsigen

cohttp: aws.byte aws.native

ocsigen: aws-ocsigen.byte

aws.byte:
	ocamlbuild -tag 'for-pack(Aws)' aws.cmo

aws.native:
	ocamlbuild -tag 'for-pack(Aws)' aws.cmx

install:
	ocamlfind install aws META _build/aws.cm*

remove: 
	ocamlfind remove aws

# Ocsigen edition

aws-ocsigen.byte:
	ocamlbuild -tags 'for-pack(Aws_ocsigen), pkg_threads, pkg_ocsigenserver' aws-ocsigen.cmo


s3c: 
	ocamlbuild s3c.native

clean:
	find . |grep '~' |xargs rm -rf
	ocamlbuild -clean