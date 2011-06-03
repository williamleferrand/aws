# Aws Makefile

# Cohttp edition

default: cohttp

all: cohttp ocsigen

cohttp: aws.byte aws.native

ocsigen: aws-ocsigen.byte aws-ocsigen.native

aws.byte:
	ocamlbuild -tag 'for-pack(Aws)' lib/aws.cmo

aws.native:
	ocamlbuild -tag 'for-pack(Aws)' lib/aws.cmx

# Ocsigen edition

aws-ocsigen.byte:
	ocamlbuild -tags 'for-pack(Aws-ocsigen), pkg_threads, pkg_ocsigenserver' lib/aws-ocsigen.cmo

aws-ocsigen.native:
	ocamlbuild -tags 'for-pack(Aws-ocsigen), pkg_threads, pkg_ocsigenserver' lib/aws-ocsigen.cmx

# Install

install-all:
	ocamlfind install aws META _build/*.cmi _build/lib/aws.cm* _build/lib/aws-ocsigen.cm*

install-cohttp:
	ocamlfind install aws META _build/lib/*.cmi _build/lib/aws.cm*

install-ocsigen:
	ocamlfind install aws META _build/lib/*.cmi _build/lib/aws-ocsigen.cm*

install: install-all

reinstall: remove install

# Command line tool

s3c: 
	ocamlbuild lib/s3c.native

# Misc tests

sdbtest.byte:
	ocamlbuild sdbtest.byte

# Cleaning 

remove: 
	ocamlfind remove aws

clean:
	find . |grep '~' |xargs rm -rf
	ocamlbuild -clean

