
# type in sources here
SOURCES = pretty.ml prioReorderer.ml bNF3.ml types.ml matcher.ml combinators.ml msg.ml reason.ml trap.ml
#ostream.ml aSCII.ml regexp.ml deterministic.ml trap.ml

MLI = pretty.mli prioReorderer.mli bNF3.mli types.mli matcher.mli combinators.mli msg.mli reason.mli trap.mli
#ostream.mli aSCII.mli regexp.mli deterministic.mli trap.mli

include $(top_srcdir)/m4/Makefile.am.vars

# CMO - all compiled objects
CMO := $(SORTED:.ml=.cmo)

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pr_o.cmo pa_checked.cmo pa_log.cmo $(LOG) pa_gt.cmo -L `camlp5o -where`"
AM_BFLAGS += -w x -rectypes -for-pack Ostap
AM_OFLAGS += -w x -rectypes -for-pack Ostap

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
CMOPK =
CMXPK =
TARGET = $(MLI) $(CMI) $(CMO)

include $(top_srcdir)/m4/Makefile.am.targets

install:

uninstall:

# overwrite some targets
install-data-am: $(TARGET)
	@true
uninstall-am: $(TARGET)
	@true

include $(top_srcdir)/m4/Makefile.am.rules
