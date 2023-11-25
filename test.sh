#!/usr/bin/env bash

set -e

spago build
spago test -m Test.Unit.Main
spago test -m Test.Snapshot.Main
