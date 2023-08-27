#!/usr/bin/env bash

set -e

spago run --main Docs.Main
nix build .#docs
open result/index.html
