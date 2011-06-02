# Aws Makefile

# Cohttp edition

all: aws.byte aws.native

aws.byte:
	ocamlbuild aws.cmo

aws.native:
	ocamlbuild aws.cmx

install:
	ocamlfind install aws META _build/aws.cm*

remove: 
	ocamlfind remove aws



s3c: 
	ocamlbuild s3c.native

clean:
	find . |grep '~' |xargs rm -rf
	ocamlbuild -clean