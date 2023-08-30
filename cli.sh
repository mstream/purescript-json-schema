#!/usr/bin/env bash

set -e

EXEC_ARGS=()
for ARG in "$@"; do
	EXEC_ARGS+=("--exec-args" "$ARG")
done

spago run --main Main.CLI "${EXEC_ARGS[@]}"
