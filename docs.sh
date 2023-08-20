#!/usr/bin/env bash

spago run --main Test.Docs
nix build .#docs
open result/index.html
