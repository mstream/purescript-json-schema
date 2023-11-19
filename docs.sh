#!/usr/bin/env bash

set -e

spago run --main Docs.Main
spago bundle-app --main Sandbox.Main --to docs/src/sandbox.js
echo '<html><head><title>Sandbox</title></head><body><script src="./sandbox.js"></script></body></html>' >docs/src/sandbox.html
nix build .#docs
