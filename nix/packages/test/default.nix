{ pkgs, ... }:
let
  script = "
    spago test
  ";
  scriptBin = pkgs.writeShellScriptBin "test" script;
in
scriptBin
