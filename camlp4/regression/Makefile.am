TESTS = test001 test002 test003 test004 test005 test006 test007 test008 test009 test010 test011 test012

TESTS_ENVIRONMENT = $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

PARSER_INCLUDES += -I $(top_srcdir)/camlp4
MODULE_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/util

EXTRA_CMA += $(PACKAGE).cmo

# OCaml parser call with flags
AM_P4FLAGS = -pp "$(CAMLP4O) $(PARSER_INCLUDES) pa_ostap.cmo"

PXFLAGS = $(P4FLAGS)
CAMLPXLIB = $(CAMLP4LIB)
MODULE_INCLUDES += -I $(CAMLP4LIB)

BFLAGS += -rectypes
OFLAGS += -rectypes

TARGET = 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log

include $(top_srcdir)/m4/Makefile.am.targets

# do not call ocamldep
.depend:
	touch .depend

# Rules to build tests
%: %.ml
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(PXFLAGS) -o $@$(EXEEXT) $<

include $(top_srcdir)/m4/Makefile.am.rules
