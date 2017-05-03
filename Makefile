SHELL := bash
.SHELLFLAGS := -ec

# The default path for the shared sandbox for all of BFWP.
BFWP_SANDBOX := $(HOME)/.local/share/bfwp/cabal-sandbox

PATH := $(BFWP_SANDBOX)/bin:$(PATH)

# Check for Brew, if found, add the PKG_CONFIG_PATH for icu from Brew.
ifneq ($(strip $(shell which brew)),)
	PKG_CONFIG_PATH += $(shell brew --prefix)/opt/icu4c/lib/pkgconfig
endif

# Use pkg-config like a civilised person to configure our compiler.
ICU_OPTS := $(shell pkg-config --libs --cflags icu-i18n icu-uc)

# Markdown sources
MDS := $(wildcard *.md)

# HTML targets
HTMLS := $(MDS:%.md=%.html)

# Pandoc options
PANDOC_OPTIONS := markdown \
	header_attributes \
	auto_identifiers \
	implicit_header_references \
	fancy_lists \
	startnum \
	table_captions \
	grid_tables \
	pandoc_title_block \
	yaml_metadata_block \
	superscript \
	subscript \
	inline_code_attributes \
	tex_math_dollars \
	shortcut_reference_links \
	implicit_figures \
	footnotes \
	inline_notes

CABAL_HADDOCK_OPTIONS := \
	--contents-location="http://localhost:8081/" \
	--all \
	--hyperlink-source \
	--hoogle

CABAL_INSTALL := \
	cabal install \
	--enable-documentation \
	$(patsubst --%,--haddock-%,$(CABAL_HADDOCK_OPTIONS))

CABAL_BUILD := \
	cabal build

CABAL_HADDOCK := \
	cabal haddock \
	$(CABAL_HADDOCK_OPTIONS)

CABAL_PRE_BINS := haddock HsColour hoogle
PRE_BINS := $(CABAL_PRE_BINS:%=$(BFWP_SANDBOX)/bin/%)

.PHONY: \
	all \
	serve \
	cabal-sandbox \
	cabal-update \
	cabal-install-deps \
	pre-bins \
	cabal-install \
	project-sandbox \
	haddock-view

# Build our project and the documentation
all: cabal-install $(HTMLS)

# We want to make sure we're using the latest cabal package list. Then
# we create the sandbox if it's not there already.
$(BFWP_SANDBOX): cabal-update
	mkdir -p $@
	(cd $@ && cabal sandbox init)

# Make sure our local sandbox config points to BFWP_SANDBOX.
project-sandbox: $(BFWP_SANDBOX)
	cabal sandbox init --sandbox=$^

cabal-update:
	cabal update

# Install our dependencies using the discovered PKG_CONFIG_PATH if
# brew is installed. Every other package manager is sensible. Also
# install haddock, hscolour, and hoogle so we can build the
# documentation.
cabal-install-deps: cabal-update project-sandbox pre-bins
	$(CABAL_INSTALL) \
		--only-dependencies \
		--gcc-options="$(ICU_OPTS)"

pre-bins: $(PRE_BINS) cabal-sandbox

$(BFWP_SANDBOX/bin/%): %
	$(CABAL_INSTALL) $@

# Build our project.
cabal-build: cabal-install-deps
	$(CABAL_BUILD)

cabal-install: cabal-build
	$(CABAL_INSTALL)

haddock-view: cabal-install
	[ -f .python-pid ] && kill $$(cat .python-pid) || true
	(echo $$$$ > .python-pid; \
	cd $(BFWP_SANDBOX)/share/doc/x86_64-linux-ghc-8.0.2/QuickTracker-1.3/html \
	&& exec python -m SimpleHTTPServer 8081 &> /dev/null < /dev/null) & \
	disown

	echo "Open http://localhost:8081/ to view local documentation."

%.html: %.md
	@pandoc --smart \
	--from=$$(echo $(PANDOC_OPTIONS) | sed 's/ \+/+/g') \
	--standalone -o $@ $^
