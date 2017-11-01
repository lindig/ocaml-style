# vim: set noet ts=8:
#
# theme(1) is part of discount, a Markdown implementation in C

all: ocaml-style.html

clean:
	rm -f ocaml-style.html

%.html: %.md
	theme $<
