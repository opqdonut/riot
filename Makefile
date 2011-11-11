#
# Riot Makefile
#

include system.mk

TOPDIR = .

DIRS = . Ginsu Riot
DOCS = README LICENSE

HSC = IConv.hsc Ginsu/Locale.hsc $(HSC_W_C)
HSC_W_C = Ginsu/CWString.hsc Curses/Curses.hsc
HSC_HS = $(subst .hsc,.hs,$(HSC))
HSC_C = $(subst .hsc,_hsc.c,$(HSC_W_C))
HSC_CO = $(subst .c,.o,$(HSC_C))
C = cbits/nomacro.c
CO = $(subst .c,.o,$(C))

INCLUDES += -I. -Icbits

all: $(HSC_HS) $(HSC_CO) $(CO)
	$(GHC) --make $(HC_OPTS) $(DEFINES) $(LIBS_CURSES) $(LIBS_ICONV) Main.hs $(HSC_CO) $(CO) -o $(PKG)

%.hs: %.hsc
	$(HSC2HS) $(DEFINES) $(INCLUDES) $<

%.o: %.c
	$(GHC) -c $(HC_OPTS) $(DEFINES) $(INCLUDES) $< -o $@
	
Ginsu/CWString_hsc.o: Ginsu/CWString_hsc.c
	$(GHC) -c $(HC_OPTS) $(DEFINES) $(INCLUDES) $< -o $@

Curses/Curses_hsc.o: Curses/Curses_hsc.c
	$(GHC) -c $(HC_OPTS) $(DEFINES) $(INCLUDES) $< -o $@

install:
	$(INSTALL_DIR) $(BINDIR)
	$(INSTALL_PROGRAM) $(PKG) $(BINDIR)
	$(INSTALL_DIR) $(DOCDIR)
	for i in $(DOCS); do \
        	$(INSTALL_DATA) $$i $(DOCDIR); \
	done

clean:
	rm -f $(PKG) $(HSC_HS) $(HSC_C)
	for i in $(DIRS); do rm -f $$i/*.o $$i/*.hi; done
