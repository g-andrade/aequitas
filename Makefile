REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
	REBAR3 = $(CURDIR)/rebar3
endif

ifdef RUNNING_ON_CI
REBAR3 = ./rebar3
else
REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")
endif

ifeq ($(REBAR3),)
	REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: all build clean check dialyzer xref
.PHONY: test test_ct test_release ci_test cover
.PHONY: console microbenchmark doc publish

.NOTPARALLEL: check test

all: build

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

clean: $(REBAR3)
	@$(REBAR3) clean

check: dialyzer xref

dialyzer: $(REBAR3)
	@$(REBAR3) dialyzer

xref: $(REBAR3)
	@$(REBAR3) xref

test: test_ct test_release

test_ct: $(REBAR3)
	@$(REBAR3) as test ct

test_release: $(REBAR3)
	./test_release.sh $(REBAR3)

ci_test: test check

cover: test
	@$(REBAR3) as test cover

console: $(REBAR3)
	@$(REBAR3) as development shell --apps aequitas

microbenchmark: $(REBAR3)
	@$(REBAR3) as development shell --script microbenchmark.escript

doc: $(REBAR3)
	@$(REBAR3) edoc

README.md: doc
	# non-portable dirty hack follows (pandoc 2.1.1 used)
	# gfm: "github-flavoured markdown"
	@pandoc --from html --to gfm doc/overview-summary.html -o README.md
	@tail -n +11 <"README.md"   >"README.md_"
	@head -n -12 <"README.md_"  >"README.md"
	@tail -n  2  <"README.md_" >>"README.md"
	@rm "README.md_"

publish: $(REBAR3)
	@$(REBAR3) hex publish
