{ pkgs, purescript-json-schema-sandbox, ... }:
let
  aspellEn = pkgs.aspellWithDicts (d: [ d.en d.en-computers d.en-science ]);
  generateDocsMain = pkgs.writeTextFile {
    name = "generate-docs.js";
    text = ''
      import { main } from "${purescript-json-schema-sandbox}/output/GenerateDocs.Main";
      main();
    '';
  };
  sandboxMain = pkgs.writeTextFile {
    name = "sandbox.js";
    text = ''
      import { main } from "${purescript-json-schema-sandbox}/output/Sandbox.Main";
      main();
    '';
  };
in
pkgs.stdenv.mkDerivation {
  nativeBuildInputs = with pkgs; [
    esbuild
    mdbook
    mdbook-linkcheck
    mdbook-mermaid
    nodePackages.markdownlint-cli
    nodejs
  ] ++ [ aspellEn ];
  buildPhase = ''
    esbuild --bundle ${sandboxMain} --outfile=docs/src/sandbox.js --platform=browser
    esbuild --bundle ${generateDocsMain} --outfile=generate-docs.js --platform=node
    node generate-docs.js
  '';
  checkPhase = ''
    function validate_spelling() {
      file=$1
      echo "checking spelling of $file"
      set +e
      cat $file | sed 's/#//g' | sed 's/[a-zA-Z0-9]\+\(_[a-zA-Z0-9]\+\)\+//g' | sed 's/```\S\+//g' | aspell pipe --dont-save-repl --lang en --mode=markdown --personal ./docs/extra-dictionary.txt --camel-case | grep '^\&';
      grep_exit_code=$?
      set -e
      if [ "$grep_exit_code" -eq 1 ]; then
        return 0
      else
        return 1
      fi
    }

    for f in ./docs/test/valid/*.md; do
      validate_spelling $f || exit 1;
    done

    for f in ./docs/test/invalid/*.md; do
      (! validate_spelling "$f") || exit 1;
    done

    markdownlint --disable MD004 -- ./docs/src

    for f in ./docs/src/*.md; do
      validate_spelling "$f" || exit 1;
    done
  '';
  doCheck = true;
  installPhase = ''
    mkdir "$out"
    mdbook build --dest-dir "$out" docs
  '';
  unpackPhase = ''
    cp -r $src/docs .
    chmod --recursive u+w docs
    mkdir docs/src
    cp docs/sandbox.css docs/src/
    cp docs/sandbox.html docs/src/
    cp $src/.markdownlint.json .
  '';
  name = "docs";
  src = ../../..;
}
