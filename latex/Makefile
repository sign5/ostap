#
# LaTeX Ostap package installation makefile.
# (C) Dmitri Boulytchev, St.Peterburg State University, 2009.
#

PHRASE="check your LaTeX installation or consider manual install"
TEXCONFIG=texconfig
REHASH=rehash

verify:
	@if which $(TEXCONFIG) &> /dev/null ; then : ; else echo "Error: texconfig not found; $(PHRASE)"; exit 1; fi

install: verify
	@echo "Starting Ostap LaTeX package installation..." ; \
	set DEST "$(DST)" ; \
	if [ -z "$(DST)" ]; then \
	   set DEST `find /usr -type d -name 'latex' 2> /dev/null | grep /tex/latex` ; \
	   if [ -z "$$2" ]; then \
	      echo "Error: failed to determine destination directory; $(PHRASE)"; \
	      exit 1 ; \
	   fi ; \
	fi ; \
	echo "Destination directory: $(DST)" ; \
	if test -d "$$2/ostap" ; then echo "Error: package Ostap already installed"; exit 1; fi ; \
	if mkdir -p "$$2/ostap" ; then : ; else exit 1 ; fi ; \
	if cp ostap.sty "$$2/ostap" ; then : ; else exit 1 ; fi ; \
	if $(TEXCONFIG) $(REHASH) ; then echo "Installation complete." ; else exit 1 ; fi