{ easy-ps, pkgs }:

let
  unpack-self = ''
    mkdir -p {.spago,src,test}
    cp $src/{packages.dhall,spago.dhall} .
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
      ""
      (builtins.attrValues deps)
  ;
  unpack-phase = deps: ''
    ${unpack-self}
    ${unpack-deps deps}
  '';
  install-phase = ''
    mkdir $out
    cp --recursive {.spago,output,src,test} $out/
  '';
  build-phase = ''
    shopt -s globstar
    build-spago-style .spago/*/*/src/**/*.purs src/**/*.purs
  '';

  mkPursLibDerivation = name: lib-path: deps:
    let
      spagoPkgs = import "${lib-path}/spago-packages.nix" { inherit pkgs; };
    in
    pkgs.stdenv.mkDerivation {
      buildPhase = build-phase;
      installPhase = install-phase;
      name = "purescript-${name}";
      nativeBuildInputs = with pkgs; [
        easy-ps.purs
        easy-ps.spago
        spagoPkgs.installSpagoStyle
        spagoPkgs.buildSpagoStyle
      ];
      src = lib-path;
      unpackPhase = unpack-phase deps;
    };

in
{ inherit mkPursLibDerivation; }
