{ easy-ps, pkgs }:
let
  mkLib =
    { browser-executables ? { }
    , deps ? { }
    , lib-path
    , name
    , node-executables ? { }
    }:
    let
      spagoPkgs = import "${lib-path}/spago-packages.nix" { inherit pkgs; };
    in
    pkgs.stdenvNoCC.mkDerivation {
      buildPhase =
        let
          browser-exec-bundle-cmds = pkgs.lib.attrsets.foldlAttrs
            (acc: bundle-name: module-name:
              ''
                ${acc}
                purs-backend-es bundle-app \
                  --main ${module-name} \
                  --platform browser \
                  --to dist/browser/${bundle-name}.js
              ''
            )
            ""
            browser-executables;
          node-exec-bundle-cmds = pkgs.lib.attrsets.foldlAttrs
            (acc: bundle-name: module-name:
              ''
                ${acc}
                purs-backend-es bundle-app \
                  --main ${module-name} \
                  --platform node \
                  --to dist/node/${bundle-name}.mjs
              ''
            )
            ""
            node-executables;
          exec-bundle-commands = browser-exec-bundle-cmds + node-exec-bundle-cmds;
        in
        ''
          set -e
          shopt -s globstar
          build-spago-style \
            deps-src/**/*.purs \
            src/**/*.purs \
            test/**/*.purs

        '' + (
          if builtins.stringLength exec-bundle-commands > 0
          then "build-spago-style --codegen corefn deps-src/**/*.purs src/**/*.purs" + exec-bundle-commands
          else ""
        );
      checkPhase = ''
        set -e
        purs-tidy check $src/{src,test}
        TEST_SRC="import {main} from './output/Test.Main/index.js'; main()"
        node --eval "$TEST_SRC" --input-type module;
      '';
      doCheck = true;
      installPhase = ''
        set -e
        mkdir $out
        cp --recursive $src/src $out/
        chmod --recursive u+w $out/src
        rsync --recursive deps-src/purs/ $out/src/purs
        ${
            if builtins.length (builtins.attrNames browser-executables) + builtins.length (builtins.attrNames node-executables) > 0
            then "cp --recursive dist $out/"
            else ""
        }
      '';
      name = "purescript-${name}";
      nativeBuildInputs = with pkgs; [
        easy-ps.purs-backend-es
        easy-ps.purs-tidy
        easy-ps.spago
        pkgs.bash
        pkgs.esbuild
        pkgs.nodejs
        pkgs.nodePackages.prettier
        pkgs.purescript
        pkgs.rsync
        spagoPkgs.buildSpagoStyle
        spagoPkgs.installSpagoStyle
      ];
      src = pkgs.lib.fileset.toSource {
        fileset = pkgs.lib.fileset.unions [
          ../../lib/purescript-${name}/spago-packages.nix
          ../../lib/purescript-${name}/src
          ../../lib/purescript-${name}/test
        ];
        root = ../../lib/purescript-${name};
      };
      unpackPhase = builtins.foldl'
        (acc: dep-src:
          ''
            ${acc}
            rsync --recursive ${dep-src}/src/purs/ deps-src/purs
          ''
        )
        ''
          set -e
          install-spago-style
          mkdir -p deps-src/purs
          rsync --recursive .spago/*/*/src/ deps-src/purs
          rm -r .spago
          cp --recursive $src/{src,test} .
        ''
        (builtins.attrValues deps);
    };
in
{ inherit mkLib; }
