#!/usr/bin/env bash

set -e

spago run --main Docs.Main
spago bundle-app --main Sandbox.Main --to docs/src/sandbox.js
cp docs/sandbox.html docs/src/
nix build .#docs
