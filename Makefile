EMACS=emacs

all: byte-compile

byte-compile:
	${EMACS} -Q -L . -batch -f batch-byte-compile fyure.el

clean:
	rm -f *.elc
