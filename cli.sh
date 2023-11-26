#!/usr/bin/env bash

set -e

spago bundle-app \
	--main Main.CLI \
	--platform node \
	--to dist/cli.mjs \
	1>dist/last-build.log 2>&1

node dist/cli.mjs "$@"
