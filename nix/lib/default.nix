{ easy-ps, pkgs }:

let
  unpack-self = ''
    set -e
    mkdir --parents {.spago,src,test}
    cp $src/{packages.dhall,spago.dhall} .
    if [ -d $src/bin ]; then
      cp --recursive $src/bin .
    fi
    if [ -d $src/scripts ]; then
      cp --recursive $src/scripts .
    fi
    if [ -d $src/snapshots ]; then
      cp --recursive $src/snapshots .
    fi
    cp --recursive $src/src/* src/
    cp --recursive $src/test/* test/
    install-spago-style
  '';
  unpack-deps = deps:
    builtins.foldl'
      (acc: dep-src:
        ''
          ${acc}
          cp --force --recursive ${dep-src}/.spago/* .spago/
          chmod --recursive u+w src
          cp --force --recursive ${dep-src}/src/* src/
        ''
      )
      "set -e"
      (builtins.attrValues deps)
  ;
  unpack-phase = deps: ''
    ${unpack-self}
    ${unpack-deps deps}
  '';
  build-phase = ''
    set -e
    shopt -s globstar
    build-spago-style .spago/*/*/src/**/*.purs src/**/*.purs test/**/*.purs
    if [ -d output/Main ]; then
      build-spago-style --codegen corefn .spago/*/*/src/**/*.purs src/**/*.purs
      purs-backend-es bundle-app --platform node --to dist/index.mjs
    fi
  '';
  check-phase = ''
    set -e
    TEST_SRC="import {main} from './output/Test.Main/index.js'; main()"
    node --eval ${TEST_SRC} --input-type module;
  '';
  fixup-phase = ''
    set -e
    if [ -f $out/output/cache-db.json ]; then
      rm $out/output/cache-db.json
    fi
  '';
  install-phase = ''
    set -e
    mkdir $out
    cp --recursive {.spago,output,src} $out/
  '';
  mkPursLibDerivation = name: lib-path: deps:
    let
      spagoPkgs = import "${lib-path}/spago-packages.nix" { inherit pkgs; };
    in
    pkgs.stdenv.mkDerivation {
      buildPhase = build-phase;
      checkPhase = check-phase;
      doCheck = true;
      fixupPhase = fixup-phase;
      installPhase = install-phase;
      name = "purescript-${name}";
      nativeBuildInputs = with pkgs; [
        easy-ps.purs
        easy-ps.purs-backend-es
        easy-ps.spago
        pkgs.esbuild
        pkgs.nodePackages.prettier
        pkgs.nodejs
        spagoPkgs.installSpagoStyle
        spagoPkgs.buildSpagoStyle
      ];
      src = lib-path;
      unpackPhase = unpack-phase deps;
    };

in
{ inherit mkPursLibDerivation; }
