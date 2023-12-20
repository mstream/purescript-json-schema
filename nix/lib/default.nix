{ easy-ps, pkgs }:

let
  unpack-self = ''
    set -e
    mkdir --parents {.spago,src,test}
    cp $src/{packages.dhall,spago.dhall} .
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
          chmod --recursive u+w {.spago,src}
          cp --force --recursive ${dep-src}/.spago/* .spago/
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
  build-phase = is-executable:
    let
      option-lines = ''
        set -e
        shopt -s globstar
      '';
      build-lines =
        if is-executable then ''
          build-spago-style --codegen corefn .spago/*/*/src/**/*.purs src/**/*.purs
          purs-backend-es bundle-app --platform node --to dist/index.mjs
        '' else "";
    in
    ''
      ${option-lines}
      build-spago-style .spago/*/*/src/**/*.purs src/**/*.purs test/**/*.purs
      ${build-lines}
    '';
  check-phase = ''
    set -e
    TEST_SRC="import {main} from './output/Test.Main/index.js'; main()"
    node --eval "$TEST_SRC" --input-type module;
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
  mkPursLibDerivation =
    { deps ? { }
    , is-executable ? false
    , lib-path
    , name
    }:
    let
      spagoPkgs = import "${lib-path}/spago-packages.nix" { inherit pkgs; };
    in
    pkgs.stdenv.mkDerivation {
      buildPhase = build-phase is-executable;
      checkPhase = check-phase;
      doCheck = true;
      fixupPhase = fixup-phase;
      installPhase = install-phase;
      name = "purescript-${name}";
      nativeBuildInputs = with pkgs; [
        easy-ps.purs-backend-es
        easy-ps.spago
        pkgs.bash
        pkgs.esbuild
        pkgs.nodePackages.prettier
        pkgs.nodejs
        pkgs.purescript
        spagoPkgs.installSpagoStyle
        spagoPkgs.buildSpagoStyle
      ];
      src = lib-path;
      unpackPhase = unpack-phase deps;
    };

in
{ inherit mkPursLibDerivation; }
