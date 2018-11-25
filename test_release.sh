#!/usr/bin/env bash
# To be invoked from Makefile

set -eux

REBAR3=$1
"$REBAR3" as test release -n test_release
_build/test/rel/test_release/bin/test_release console <<<"q()."
