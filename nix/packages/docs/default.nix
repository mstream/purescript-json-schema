{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; [ esbuild mdbook ];
  installPhase = ''
    mkdir "$out"
    cp -r $src/docs .
    mdbook build --dest-dir "$out" docs
  '';
  name = "purescript-json-schema-docs";
  src = ../../..;
}
