
# Makefile to capture configuration and build procedure(s)

FLAGS=
INSTDIR=		# Set on command-line

# Development compile target; build quickly, no optimization

all: release 

# Optimized build
opt:
	make FLAGS="-O2 $(FLAGS)" informash

# Development build
dev: informash

install: dircheck release
	install -d "$(INSTDIR)/bin/" "$(INSTDIR)/share/man/man1"
	install -cs informash "$(INSTDIR)/bin/"
	install -c -m 644 informash.1 "$(INSTDIR)/share/man/man1"

cabal-deps:
	cabal update
	cabal install binary-0.8.3.0 pureMD5-2.1.3 

help:
	@echo "Public targets:"
	@echo "    make cabal-deps         - Install dependencies"
	@echo "    make dev	[FLAGS=...]    - compile unoptimized (for development)"
	@echo "    make opt                - compile optimized"
	@echo "    make install INSTDIR=...- install in specified location."
	@echo

### ----------------------------------------------------------------------

release: opt informash.1

dircheck:
	echo "Ensuring that INSTDIR is set and present..."
	@[ -d "$(INSTDIR)" ] && echo "Directory $(INSTDIR) exists."

informash : informash.hs Histogram.hs TextStats.hs Token.hs Util.hs \
			WordRamp.hs WordTable.hs
	ghc $(FLAGS) --make informash.hs

informash.1 : informash.pod
	pod2man --release=""  --center="informash" informash.pod > informash.1

clean:
	-rm *.hi *.o informash informash.1
