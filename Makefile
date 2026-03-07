.POSIX:

MAKEFLAGS += k
EMACS ?= emacs

EMACSFLAGS      = -Q -batch -L .
COMPILE_COMMAND = -f batch-byte-compile

ELS  = kawacode-pipe.el kawacode-list-pipe.el kawacode-process-sockets.el kawacode.el
ELCS = $(ELS:.el=.elc)

.PHONY: compile clean

%.elc: %.el
	@printf "Compiling $<\n"
	$(EMACS) $(EMACSFLAGS) $(COMPILE_COMMAND) $<

compile: $(ELCS)

clean:
	@rm -f *.elc
