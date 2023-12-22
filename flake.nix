{
  description = "Purescript JSON schema library";

  inputs = {
    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix/master";
    flake-utils.url = "github:numtide/flake-utils/main";
    nixpkgs.url = "github:nixos/nixpkgs/23.11";
  };

  outputs =
    { easy-purescript-nix
    , flake-utils
    , nixpkgs
    , ...
    }:
    let
      name = "purescript-json-schema-monorepo";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];

    in
    flake-utils.lib.eachSystem supportedSystems (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      easy-ps = import easy-purescript-nix { inherit pkgs; };

      lint-nix = pkgs.stdenvNoCC.mkDerivation {
        checkPhase = ''
          deadnix --exclude ./node_modules/lodash/flake.nix $src
          statix check --ignore 'spago-packages.nix' $src
        '';
        doCheck = true;
        dontBuild = true;
        installPhase = ''
          mkdir "$out"
        '';
        name = "lint-nix";
        nativeBuildInputs = with pkgs; [ deadnix statix ];
        src = ./nix;
      };

      lib = import ./nix/lib { inherit easy-ps pkgs; };

      purescript-computation = lib.mkLib {
        deps = {
          inherit
            purescript-docs
            purescript-markdown
            purescript-utils;
        };
        lib-path = ./lib/purescript-computation;
        name = "computation";
      };

      purescript-docs = lib.mkLib {
        deps = { inherit purescript-markdown purescript-utils; };
        lib-path = ./lib/purescript-docs;
        name = "docs";
      };

      purescript-docs-sandbox = lib.mkLib {
        deps = { inherit purescript-json-schema; };
        lib-path = ./lib/purescript-docs-sandbox;
        name = "docs-sandbox";
      };

      purescript-json-schema = lib.mkLib {
        deps = {
          inherit
            purescript-computation
            purescript-docs
            purescript-markdown
            purescript-utils;
        };
        lib-path = ./lib/purescript-json-schema;
        name = "json-schema";
      };

      purescript-json-schema-cli = lib.mkLib {
        deps = {
          inherit
            purescript-docs-sandbox
            purescript-optparse
            purescript-utils;
        };
        lib-path = ./lib/purescript-json-schema-cli;
        name = "json-schema-cli";
        node-executables = { index = "Main"; };
      };

      purescript-json-schema-sandbox = lib.mkLib {
        browser-executables = {
          sandbox = "Sandbox.Main";
        };
        deps = {
          inherit
            purescript-docs-sandbox
            purescript-json-schema
            purescript-json-schema-cli;
        };
        lib-path = ./lib/purescript-json-schema-sandbox;
        name = "json-schema-sandbox";
        node-executables = { generate-docs = "GenerateDocs.Main"; };
      };

      purescript-markdown = lib.mkLib {
        deps = { inherit purescript-utils; };
        lib-path = ./lib/purescript-markdown;
        name = "markdown";
      };

      purescript-optparse = lib.mkLib {
        lib-path = ./lib/purescript-optparse;
        name = "optparse";
      };

      purescript-utils = lib.mkLib {
        lib-path = ./lib/purescript-utils;
        name = "utils";
      };

      docs = import ./nix/packages/docs {
        inherit pkgs purescript-json-schema-sandbox;
      };

      devShellInputs = {
        easy-ps = with easy-ps; [
          psa
          purs-backend-es
          purs-tidy
          spago
          spago2nix
        ];

        node-packages = with pkgs.nodePackages; [ prettier ];

        pkgs = with pkgs; [
          act
          bash
          docker
          esbuild
          gh
          git
          httplz
          imagemagick
          mdbook
          nodejs
          purescript
        ];
      };

      serve = flake-utils.lib.mkApp {
        drv =
          pkgs.runCommandCC
            "serve"
            { }
            ''
              mkdir $out
              ${pkgs.httplz}/bin/httplz ${docs}/html/
            '';
      };

      workflow = flake-utils.lib.mkApp {
        drv = pkgs.act;
      };

    in
    {
      apps = { inherit serve workflow; };
      checks = { inherit docs lint-nix; };
      devShells.default = pkgs.mkShell {
        inherit name;
        buildInputs =
          devShellInputs.easy-ps ++
          devShellInputs.node-packages ++
          devShellInputs.pkgs;
        inputsFrom = [ docs lint-nix ];
        shellHook = ''
          PS1="\[\e[33m\][\[\e[m\]\[\e[34;40m\]${name}:\[\e[m\]\[\e[36m\]\w\[\e[m\]\[\e[33m\]]\[\e[m\]\[\e[32m\]\\$\[\e[m\] "
        '';
      };
      packages = {
        inherit
          docs
          purescript-computation
          purescript-docs
          purescript-docs-sandbox
          purescript-json-schema
          purescript-json-schema-cli
          purescript-json-schema-sandbox
          purescript-markdown
          purescript-optparse
          purescript-utils;
      };
    }
    );
}
