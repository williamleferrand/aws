# Aws Makefile

aws:
	ocamlbuild aws.cmo

clean:
	find . |grep '~' |xargs rm -rf
	ocamlbuild -clean