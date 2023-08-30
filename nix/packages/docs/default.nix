{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; [ esbuild mdbook mdbook-mermaid nodePackages.markdownlint-cli ];
  checkPhase = ''
    markdownlint docs/src
  '';
  doCheck = true;
  installPhase = ''
    mkdir "$out"
    mdbook build --dest-dir "$out" docs
  '';
  unpackPhase = ''
    cp -r $src/docs .
    cp -r $src/.markdownlint.json .
  '';
  name = "purescript-json-schema-docs";
  src = ../../..;
}
