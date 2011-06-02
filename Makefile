# Aws Makefile

# Cohttp edition

default: cohttp

all: cohttp ocsigen

cohttp: aws.byte aws.native

ocsigen: aws-ocsigen.byte aws-ocsigen.native

aws.byte:
	ocamlbuild -tag 'for-pack(Aws)' aws.cmo

aws.native:
	ocamlbuild -tag 'for-pack(Aws)' aws.cmx

# Ocsigen edition

aws-ocsigen.byte:
	ocamlbuild -tags 'for-pack(Aws-ocsigen), pkg_threads, pkg_ocsigenserver' aws-ocsigen.cmo

aws-ocsigen.native:
	ocamlbuild -tags 'for-pack(Aws-ocsigen), pkg_threads, pkg_ocsigenserver' aws-ocsigen.cmx

# Install

install-all:
	ocamlfind install aws META _build/aws.cm* _build/aws-ocsigen.cm*

install-cohttp:
	ocamlfind install aws META _build/aws.cm*

install-ocsigen:
	ocamlfind install aws META _build/aws-ocsigen.cm*

install: install-all

reinstall: remove install

# Command line tool

s3c: 
	ocamlbuild s3c.native

# Cleaning 

remove: 
	ocamlfind remove aws

clean:
	find . |grep '~' |xargs rm -rf
	ocamlbuild -clean

