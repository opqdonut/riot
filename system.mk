#
# System-specific settings
#

# Paths

PKG=		riot
PREFIX=         /usr/local
BINDIR=         $(PREFIX)/bin
LIBDIR=         $(PREFIX)/lib/$(PKG)
DOCDIR=         $(PREFIX)/share/doc/$(PKG)

#
# Character sets. If your iconv implementation is lacking (glibc is ok),
# comment out the CF_CHARSET_SUPPORT line. If your system doesn't support 
# wchar_t or wchar_t isn't iso-10646, comment-out the CF_WCHAR_SUPPORT line.
# Usually, if you do one of these, you want to do the other as well, as the
# first option controls conversion between mbox and internal Unicode 
# presentation, and the second from Unicode to locale's encoding for display.
# If you unset CF_WCHAR_SUPPORT, you also should not use ncursesw above.
#
 
DEFINES += -DCF_CHARSET_SUPPORT
DEFINES += -DCF_WCHAR_SUPPORT

#
# Curses. Use ncursesw if you want widechar support.
#
DEFINES += -DHAVE_WADDNWSTR -DHAVE_RESIZETERM
LIBS_CURSES = -lncursesw
#LIBS_CURSES = -lncurses


#LIBS_ICONV =
# OpenBSD needs:
#LIBS_ICONV = -liconv -L/usr/local/lib

# 
# Define this if your <langinfo.h> doesn't provide the CODESET value
# (OpenBSD, at least)
#
#DEFINES += -DNO_LANGINFO_CODESET

#
# Define this if you have a broken
# System.Posix.Signals.setStoppedChildFlag (OpenBSD, at least)
#
#DEFINES += -DBROKEN_NOCLDSTOP

# GHC and tools

DEFINES += -DGHC64

GHC=            ghc
HC_OPTS=        -fglasgow-exts

#HC_OPTS+=      -Onot -fasm -H64m
HC_OPTS+=       -cpp -fasm -funbox-strict-fields -fignore-asserts
HC_OPTS+=       -cpp
#HC_OPTS+=      -debug

HSC2HS=         hsc2hs

INSTALL=        sh $(TOPDIR)/install-sh -c
INSTALL_PROGRAM=$(INSTALL) -m 755
INSTALL_DATA=   $(INSTALL) -m 644
INSTALL_DIR=    mkdir -p

