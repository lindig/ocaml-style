# vim: set noet ts=8:
#
# theme(1) is part of discount, a Markdown implementation in C

all: README.html

clean:
	rm -f README.html

%.html: %.md
	theme $<
