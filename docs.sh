#!/usr/bin/env bash

set -e

spago run --main Test.Docs
nix build .#docs
open result/index.html
