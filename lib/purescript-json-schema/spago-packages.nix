# This file was generated by Spago2Nix

{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {

    "aff" = pkgs.stdenv.mkDerivation {
        name = "aff";
        version = "v7.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-aff.git";
          rev = "6adec6ff048a7876f74c294c440374cd21342d39";
          sha256 = "1viplap030ym9ya033xl6x41hvdc12v9ngwp1v64ayl40a5m1d47";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "ansi" = pkgs.stdenv.mkDerivation {
        name = "ansi";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/hdgarrood/purescript-ansi.git";
          rev = "7d898732d643a977a78004851112a4417909e126";
          sha256 = "1aml84m5p1s14kj29m39182byg44nf275p4cinx3kgwhv1gj0qcp";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "argonaut-codecs" = pkgs.stdenv.mkDerivation {
        name = "argonaut-codecs";
        version = "v9.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-argonaut-codecs.git";
          rev = "f8fdc1e34142fa84e66022ea5d417a008d709146";
          sha256 = "1ncpl512k2xdaf7r5ixwxkm6i4vym7a6a3ih71z489h9ad781q73";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "argonaut-core" = pkgs.stdenv.mkDerivation {
        name = "argonaut-core";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-argonaut-core.git";
          rev = "68da81dd80ec36d3b013eff46dc067a972c22e5d";
          sha256 = "0hxl17ddbflkk0hchjgk5xj6j9fwp3b182w4g073p0dwscdl4f08";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "arraybuffer-types" = pkgs.stdenv.mkDerivation {
        name = "arraybuffer-types";
        version = "v3.0.2";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-arraybuffer-types.git";
          rev = "9b0b7a0f9ee034e039f3d3a2a9c3f74eb7c9264a";
          sha256 = "1q3111jk6jj64bxy86lc7ik6q5mgdz87bc086wj5rm57dmlivlxx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "arrays" = pkgs.stdenv.mkDerivation {
        name = "arrays";
        version = "v7.3.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-arrays.git";
          rev = "6554b3d9c1ebb871477ffa88c2f3850d714b42b0";
          sha256 = "0hli942mp7wwxyx2h5ijdk2cz6g1njhrsm0bl6xwc8l2s68flj82";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "avar" = pkgs.stdenv.mkDerivation {
        name = "avar";
        version = "v5.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-avar.git";
          rev = "d00f5784d9cc8f079babd62740f5c52b87e5caa5";
          sha256 = "132g0ccmjr8328xpaycww8b5wmx1vi3li4irxxqcjk5wi47i8c9h";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "bifunctors" = pkgs.stdenv.mkDerivation {
        name = "bifunctors";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-bifunctors.git";
          rev = "16ba2fb6dd7f05528ebd9e2f9ca3a068b325e5b3";
          sha256 = "1qz5aaiq5w4g1rhjb63z47h6nqhw8hw23x1zw5alam047pq4vfkg";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "catenable-lists" = pkgs.stdenv.mkDerivation {
        name = "catenable-lists";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-catenable-lists.git";
          rev = "09abe1f4888bc00841ad2b59e56a9e7ce7ebd4ab";
          sha256 = "0fn6caspdnb0nrlj31sy7d5hxrhndfz7a2b9wfkxf5bv5vpjyawb";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

  # computation is a Local package in (Local "./../../lib/purescript-computation")

    "console" = pkgs.stdenv.mkDerivation {
        name = "console";
        version = "v6.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-console.git";
          rev = "dc9854023e3392399f5eea9f5db965dbfe2cfe11";
          sha256 = "0br5i4kzya2gwvlakbqzy4ivav9fpp61ipfpch45jnhqqfgfdhmd";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "const" = pkgs.stdenv.mkDerivation {
        name = "const";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-const.git";
          rev = "ab9570cf2b6e67f7e441178211db1231cfd75c37";
          sha256 = "0mcpwqqf5bcwxlzk53smvsjjz1ymlnq0ypphg6sc83ibw70g64f6";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "contravariant" = pkgs.stdenv.mkDerivation {
        name = "contravariant";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-contravariant.git";
          rev = "9ad3e105b8855bcc25f4e0893c784789d05a58de";
          sha256 = "1xkd3rfs8v20w7mj6sj6dmhfb2vb1zpmjv245xj1rahk3g16qm7m";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "control" = pkgs.stdenv.mkDerivation {
        name = "control";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-control.git";
          rev = "a6033808790879a17b2729e73747a9ed3fb2264e";
          sha256 = "05sdgywprwpav62d2bvlb80yd39brxhnmhhl1f116mm9hw5clqia";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "datetime" = pkgs.stdenv.mkDerivation {
        name = "datetime";
        version = "v6.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-datetime.git";
          rev = "7f6062346055e654942caed6c44612b59031f059";
          sha256 = "0z5f62arrli0dgmggv6z2qkv1qc9isdivcn7njywsa8wmc4vd47n";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "debug" = pkgs.stdenv.mkDerivation {
        name = "debug";
        version = "v6.0.2";
        src = pkgs.fetchgit {
          url = "https://github.com/garyb/purescript-debug.git";
          rev = "a37b82f62c89734934c0962ece3412913020460a";
          sha256 = "0q3r21zh0kjy9aq8q2siaf8841q63cmxjl2fbk0z3r0z4cfihccg";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "distributive" = pkgs.stdenv.mkDerivation {
        name = "distributive";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-distributive.git";
          rev = "6005e513642e855ebf6f884d24a35c2803ca252a";
          sha256 = "0x6cfsx9ff66kvw9l10pxd5vv16r91mmfxrfc68w1xfdfi81lhdx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

  # docs is a Local package in (Local "./../../lib/purescript-docs")

    "effect" = pkgs.stdenv.mkDerivation {
        name = "effect";
        version = "v4.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-effect.git";
          rev = "a192ddb923027d426d6ea3d8deb030c9aa7c7dda";
          sha256 = "0aa10lc6h9mlf4xf3g3ziig7v6kxdqvbh20kma8ay59w0b1bhmj1";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "either" = pkgs.stdenv.mkDerivation {
        name = "either";
        version = "v6.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-either.git";
          rev = "af655a04ed2fd694b6688af39ee20d7907ad0763";
          sha256 = "05zps4klvgmvlm06f4hrycssm8q0pysbqnjsrk26lfvmid6mmg63";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "enums" = pkgs.stdenv.mkDerivation {
        name = "enums";
        version = "v6.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-enums.git";
          rev = "cd373c580b69fdc00e412bddbc299adabe242cc5";
          sha256 = "1wk98ddmgyii1ifqawnhqkiqlrs405qcp3m12jsg1yp8jv6ppv47";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "exceptions" = pkgs.stdenv.mkDerivation {
        name = "exceptions";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-exceptions.git";
          rev = "afab3c07c820bb49b6c5be50049db46a964a6161";
          sha256 = "04xrbrcjwsv8gj465ygrlqhv1majxmr6m242iazg6apxlr89hgvg";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "exists" = pkgs.stdenv.mkDerivation {
        name = "exists";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-exists.git";
          rev = "f765b4ace7869c27b9c05949e18c843881f9173b";
          sha256 = "0xnxi8fsmi15wrpq5d99gg94glk8a45hpx7pf4pmxg5r6rn1vjvx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "foldable-traversable" = pkgs.stdenv.mkDerivation {
        name = "foldable-traversable";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-foldable-traversable.git";
          rev = "b3926f870532d287ea59e2d5cd3873b81ef2a93a";
          sha256 = "0xg8qvyc6r9wqgy7wnw1rjqljl4wpgdrkxsm5x9rsagj2k5brxip";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "foreign" = pkgs.stdenv.mkDerivation {
        name = "foreign";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-foreign.git";
          rev = "2dd222d1ec7363fa0a0a7adb0d8eaf81bb7006dd";
          sha256 = "0ycjaal8b2rsg3zl3b7acv166vjfdvziiza3fs8nhfw0rx0xmm1m";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "foreign-object" = pkgs.stdenv.mkDerivation {
        name = "foreign-object";
        version = "v4.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-foreign-object.git";
          rev = "9bfb4eb6271b151414594cfec669fb4b18b91bd1";
          sha256 = "1fr5hwzkan7yv2kbgvii2b8kanhjkqrbixcyj7kyv9fwkk8dk96a";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "fork" = pkgs.stdenv.mkDerivation {
        name = "fork";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-fork.git";
          rev = "a5c3bc6f357e97669e8c29c6f79f5f55be0d42c0";
          sha256 = "0yzh55vfrhrr660ni3sbbljypjirbka5jlks4zibqmhwrmy3wnj9";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "free" = pkgs.stdenv.mkDerivation {
        name = "free";
        version = "v7.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-free.git";
          rev = "068ad8c146a4fc48dd896c58384c312b04b06a74";
          sha256 = "0hv7jn8lj7m0q54r5xbmv6zfg9nclx4yfrfl2vqbqcir04518r87";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "functions" = pkgs.stdenv.mkDerivation {
        name = "functions";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-functions.git";
          rev = "f626f20580483977c5b27a01aac6471e28aff367";
          sha256 = "1bsf2y8hx103a92yrc6m05q2dv51ckag1srd7q5n9vaf2k9byrj9";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "functors" = pkgs.stdenv.mkDerivation {
        name = "functors";
        version = "v5.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-functors.git";
          rev = "022ffd7a2a7ec12080314f3d217b400674a247b4";
          sha256 = "1pnw3r1nk0rx8mrw3ajjml40zvx9v26qrqwj77pz7hpwxigxfbff";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "gen" = pkgs.stdenv.mkDerivation {
        name = "gen";
        version = "v4.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-gen.git";
          rev = "9fbcc2a1261c32e30d79c5418edef4d96fe76931";
          sha256 = "0kajzcp9a8dmqvb3ga27hlf18m2smbp98a0bmzzb9vv156ckw0b4";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "heterogeneous" = pkgs.stdenv.mkDerivation {
        name = "heterogeneous";
        version = "v0.6.0";
        src = pkgs.fetchgit {
          url = "https://github.com/natefaubion/purescript-heterogeneous.git";
          rev = "5b7542bdd9547c5aef95b8878f103f0ce4ee1383";
          sha256 = "0ff1wjq2ayhy67ydggbb2qlzbhvmv1m1z8dcp48p9zhxiq9fpfsz";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "identity" = pkgs.stdenv.mkDerivation {
        name = "identity";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-identity.git";
          rev = "ef6768f8a52ab0bc943a85f5761ba07c257f639f";
          sha256 = "1lh0pkwwdyd8q9bgf4l1m96gdnardki4r2s359cnhrwp94b43fyr";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "integers" = pkgs.stdenv.mkDerivation {
        name = "integers";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-integers.git";
          rev = "54d712b25c594833083d15dc9ff2418eb9c52822";
          sha256 = "04j9sqvkgyxxvbm8jfc2118wzmgswn8jva9fysgbsnmsg9vcxfm6";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "invariant" = pkgs.stdenv.mkDerivation {
        name = "invariant";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-invariant.git";
          rev = "1d2a196d51e90623adb88496c2cfd759c6736894";
          sha256 = "07rqag47ykdiqc5yfbc1a64ijaqliljhcw1vgbv81m7ljzzd2xna";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "js-date" = pkgs.stdenv.mkDerivation {
        name = "js-date";
        version = "v8.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-js-date.git";
          rev = "1ea020316946cc4b87195bca9c54d0c16abaa490";
          sha256 = "03535g8hc4bzyzig25hf75dls9ypsgrdk2nc45h8zw4xyy4ha0gz";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "lazy" = pkgs.stdenv.mkDerivation {
        name = "lazy";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-lazy.git";
          rev = "48347841226b27af5205a1a8ec71e27a93ce86fd";
          sha256 = "0dxlc3b2bdqsri45i7a9bvwvv0gg43cc1gq61zi41f290a29ny2q";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "lcg" = pkgs.stdenv.mkDerivation {
        name = "lcg";
        version = "v4.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-lcg.git";
          rev = "67c6c6483a563a59ae036d9dca0f1be2835326a5";
          sha256 = "1shzn6zvc1cxd7v0bvfsk7x3xf59vxby8c5lfjvd746r6396zn65";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "lists" = pkgs.stdenv.mkDerivation {
        name = "lists";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-lists.git";
          rev = "b113451e5b41cad87d669a3165f955c71cd863e2";
          sha256 = "1mg7vy44k8jlqkri7x4ikciixx92b17wi35887x09dwdsicjf0sx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

  # markdown is a Local package in (Local "./../../lib/purescript-markdown")

    "maybe" = pkgs.stdenv.mkDerivation {
        name = "maybe";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-maybe.git";
          rev = "c6f98ac1088766287106c5d9c8e30e7648d36786";
          sha256 = "0n33g8c579vcs06ii0r7f14jjdwzjw7p583w4nmrv9h442q2cvlz";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "mmorph" = pkgs.stdenv.mkDerivation {
        name = "mmorph";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/Thimoteus/purescript-mmorph.git";
          rev = "94bc558ac34184d5236a7a9b2463dcc7551ced8e";
          sha256 = "0m3m1kjy4i9lwzj8l5qlafxawyhbdxj035wa5ys4j8v4013a92xg";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "newtype" = pkgs.stdenv.mkDerivation {
        name = "newtype";
        version = "v5.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-newtype.git";
          rev = "29d8e6dd77aec2c975c948364ec3faf26e14ee7b";
          sha256 = "0kfc644zn2f9rpx8fy4gjvmz04k9mv1c2xrqis8m3s0ydnhbffd7";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "node-buffer" = pkgs.stdenv.mkDerivation {
        name = "node-buffer";
        version = "v9.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-node/purescript-node-buffer.git";
          rev = "8263037a2db02823a0b47c57a80214382568bf7c";
          sha256 = "1i7d34w7fhfs2sir48f6s0f7703pa2x55yahx6xd490lyxcg2w13";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "node-event-emitter" = pkgs.stdenv.mkDerivation {
        name = "node-event-emitter";
        version = "v3.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-node/purescript-node-event-emitter.git";
          rev = "b283c3eb6abc32a88fd8876af746b9548e78d93f";
          sha256 = "1hgj94b238ncj5gd6ixb23l4yykwvk3r7574pprv7hsw6vchy9rl";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "node-fs" = pkgs.stdenv.mkDerivation {
        name = "node-fs";
        version = "v9.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-node/purescript-node-fs.git";
          rev = "b210f7b12897be8ca9e17b7885d4daff4c2f2277";
          sha256 = "1bwlzsaam8isng9z1xvmg05ls761nizq1gps1a93plj8m0nfbf4h";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "node-path" = pkgs.stdenv.mkDerivation {
        name = "node-path";
        version = "v5.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-node/purescript-node-path.git";
          rev = "d5f08cfde829b831408c4c6587cec83f2cd6a58e";
          sha256 = "1d38ipij4lb1nq8wkv7kmpk77z5ac0spfc89hfc9yn6jnscl6sky";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "node-streams" = pkgs.stdenv.mkDerivation {
        name = "node-streams";
        version = "v9.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-node/purescript-node-streams.git";
          rev = "8aaec35f1c6316924e360274cd248edee01eae4f";
          sha256 = "1p9f8vz29nk3jbwzkhvx0kr8fgwf6dayjbdrkklqg6r9fl7hvv04";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "nonempty" = pkgs.stdenv.mkDerivation {
        name = "nonempty";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-nonempty.git";
          rev = "28150ecc7419238b187abd609a92a645273348bb";
          sha256 = "1mwdc2sny2ygp67kyk7h2493vq7syf7j18s3d9a5gwf1y7jlf7gh";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "now" = pkgs.stdenv.mkDerivation {
        name = "now";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-now.git";
          rev = "b5ffed2381e5fefc063f484e607e8499e79eaf32";
          sha256 = "01khmqs4iz7dhlvd43zxj19q0c0vvqf1m128vgvgxz5mv430zfgr";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "nullable" = pkgs.stdenv.mkDerivation {
        name = "nullable";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-contrib/purescript-nullable.git";
          rev = "3202744c6c65e8d1fbba7f4256a1c482078e7fb5";
          sha256 = "10s3b54pjmr2rxlyzvc8yc99k1dpn0qrijnc4y65yj5z6bxf2791";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "numbers" = pkgs.stdenv.mkDerivation {
        name = "numbers";
        version = "v9.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-numbers.git";
          rev = "27d54effdd2c0e7a86fe356b1cd813dca5981c2d";
          sha256 = "0qx52ajzz8i1ry0mcwkdxzm7lybd05zia8asl0n87hamdz80ilxd";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "ordered-collections" = pkgs.stdenv.mkDerivation {
        name = "ordered-collections";
        version = "v3.1.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-ordered-collections.git";
          rev = "313ec0c3556854cd4e4752a77a86b102fc41ad05";
          sha256 = "1n19awmjg91wiqy28lhiyyiyblqp4f5yl0w1v10vpi0h6pc80v8c";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "orders" = pkgs.stdenv.mkDerivation {
        name = "orders";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-orders.git";
          rev = "f86db621ec5eef1274145f8b1fd8ebbfe0ed4a2c";
          sha256 = "14fjls3v14ia6fr2w4p0i5dqv10zk343wg28m4c77a17jmvbs9r4";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "parallel" = pkgs.stdenv.mkDerivation {
        name = "parallel";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-parallel.git";
          rev = "a6be752e44f115be1ec4d742f307bf7c0e14a765";
          sha256 = "0xm2bajh7cal5iqv7fvfi4w46imr0jhmcxgav8rljx3priqf35fx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "partial" = pkgs.stdenv.mkDerivation {
        name = "partial";
        version = "v4.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-partial.git";
          rev = "0fa0646f5ea1ec5f0c46dcbd770c705a6c9ad3ec";
          sha256 = "04s1h0r3slyd8kcamhqqrr6piksl9y76nmf2418j0ifhp16qwxmm";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "pipes" = pkgs.stdenv.mkDerivation {
        name = "pipes";
        version = "v8.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/felixschl/purescript-pipes.git";
          rev = "e3bdc0b0db0a67e89a717b6118b23e78a380e23f";
          sha256 = "1jz3bfbl8b0hgbcm9y98bv2z29b072v1k8snp19vb2xql1zd40sx";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "prelude" = pkgs.stdenv.mkDerivation {
        name = "prelude";
        version = "v6.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-prelude.git";
          rev = "f4cad0ae8106185c9ab407f43cf9abf05c256af4";
          sha256 = "0j6mb9w728ifcp10jdv7l9k7k5pw8j1f0fa7xyb8xmbxzc59xqpy";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "profunctor" = pkgs.stdenv.mkDerivation {
        name = "profunctor";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-profunctor.git";
          rev = "0a966a14e7b0c827d44657dc1710cdc712d2e034";
          sha256 = "07rhk8micbc74my6s53xbj2smvvwah2w1drqrcmcnncxdks3lqg3";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "quickcheck" = pkgs.stdenv.mkDerivation {
        name = "quickcheck";
        version = "v8.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-quickcheck.git";
          rev = "bf5029f97e6c0d7552d3a08d2ab793a19e2c5e3d";
          sha256 = "142dvh57fl8b6i7mm37a38v7vkc1znbqz6l6wqa704m53hkikvyd";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "random" = pkgs.stdenv.mkDerivation {
        name = "random";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-random.git";
          rev = "9540bc965a9596da02fefd9949418bb19c92533a";
          sha256 = "0qz14qviz5053j9h1jwpk0gnn692hcdx6fp90wckzrnw31d53174";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "record" = pkgs.stdenv.mkDerivation {
        name = "record";
        version = "v4.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-record.git";
          rev = "c89cd1ada6b636692571fc374196b1c39c4c9f70";
          sha256 = "1g7s2h1as5cz824wpm0jhjprrh66shha5i4gq37q73yw0s5p2ahm";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "refs" = pkgs.stdenv.mkDerivation {
        name = "refs";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-refs.git";
          rev = "f8e6216da4cb9309fde1f20cd6f69ac3a3b7f9e8";
          sha256 = "09bvfxhjfwfwv55py45s71maazwrr68k0rk4v8ynfqv91h34319h";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "safe-coerce" = pkgs.stdenv.mkDerivation {
        name = "safe-coerce";
        version = "v2.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-safe-coerce.git";
          rev = "7fa799ae80a38b8d948efcb52608e58e198b3da7";
          sha256 = "00m4l733gpl0153cbl6n5kly7jr8ids399apza2rbczif40brp9g";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "spec" = pkgs.stdenv.mkDerivation {
        name = "spec";
        version = "v7.5.5";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript-spec/purescript-spec.git";
          rev = "61d03e67d9981e5b21645026b199eabe6c33b3cb";
          sha256 = "0apalmiq92id0dka5i07bgqalbirmlf5ik401my44x8qnk91h4gq";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "st" = pkgs.stdenv.mkDerivation {
        name = "st";
        version = "v6.2.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-st.git";
          rev = "fc2fe2972bb12e6a2bd3b295baf01577240c23ac";
          sha256 = "17syc11gxhi1law4lskrr4swr62n4r7irj5imdyjjp8z0p5c6p8z";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "strings" = pkgs.stdenv.mkDerivation {
        name = "strings";
        version = "v6.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-strings.git";
          rev = "3d3e2f7197d4f7aacb15e854ee9a645489555fff";
          sha256 = "1dx6l4j4yw6w6nxa687gf04q4caa99ccl4cp4q22nda6ghsz7yjl";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "tailrec" = pkgs.stdenv.mkDerivation {
        name = "tailrec";
        version = "v6.1.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-tailrec.git";
          rev = "5661a10afbd4849bd2e45139ea567beb40b20f9f";
          sha256 = "0snhrvkpd429r0d0bzs0mxwwz3am9bpa1m9f5a9hpmyjjkl7gddw";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "transformers" = pkgs.stdenv.mkDerivation {
        name = "transformers";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-transformers.git";
          rev = "be72ab52357d9a665cbf93d73ba1c07c4b0957ee";
          sha256 = "0ijrdsppl8vx0rlbwc9p0vjbmvwlcy4ia7xymvk6y34zbxzjlzm6";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "tuples" = pkgs.stdenv.mkDerivation {
        name = "tuples";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-tuples.git";
          rev = "4f52da2729b448c8564369378f1232d8d2dc1d8b";
          sha256 = "1m1ng0xxicb73945jymcl1hn2y2hmynlnmhb2k0kkn1jrjwgcc3d";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "type-equality" = pkgs.stdenv.mkDerivation {
        name = "type-equality";
        version = "v4.0.1";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-type-equality.git";
          rev = "0525b7d39e0fbd81b4209518139fb8ab02695774";
          sha256 = "1ass38jdycsjisdimdc4drg2w8vkkwp6lkvz3kvy7q0h98vdmlbr";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "typelevel-prelude" = pkgs.stdenv.mkDerivation {
        name = "typelevel-prelude";
        version = "v7.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-typelevel-prelude.git";
          rev = "dca2fe3c8cfd5527d4fe70c4bedfda30148405bf";
          sha256 = "0x86mrg33kpnrnsfp4p3c92j5lpyqzy87bxdynwf7smk3inqr2jc";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "unfoldable" = pkgs.stdenv.mkDerivation {
        name = "unfoldable";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-unfoldable.git";
          rev = "493dfe04ed590e20d8f69079df2f58486882748d";
          sha256 = "15z2k639ph8wdkrc2y838m5am1z7szw2vqymmv021skzisyn7zwf";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

    "unsafe-coerce" = pkgs.stdenv.mkDerivation {
        name = "unsafe-coerce";
        version = "v6.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/purescript/purescript-unsafe-coerce.git";
          rev = "ab956f82e66e633f647fb3098e8ddd3ec58d689f";
          sha256 = "0r6d3dx8jalfzvrvkagz9v05yxwkkhgbzlpswg4w1cyl03zjcla4";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

  # utils is a Local package in (Local "./../../lib/purescript-utils")

    "variant" = pkgs.stdenv.mkDerivation {
        name = "variant";
        version = "v8.0.0";
        src = pkgs.fetchgit {
          url = "https://github.com/natefaubion/purescript-variant.git";
          rev = "8b2bd73a4202257d80a955c4dd36f6055f149823";
          sha256 = "18nyxnaqqg1zzydwd2hh9hbzr31j509g5yb1lnd0m60bma4l9h5g";
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

  };

  cpPackage = pkg:
    let
      target = ".spago/${pkg.name}/${pkg.version}";
    in ''
      if [ ! -e ${target} ]; then
        echo "Installing ${target}."
        mkdir -p ${target}
        cp --no-preserve=mode,ownership,timestamp -r ${toString pkg.outPath}/* ${target}
      else
        echo "${target} already exists. Skipping."
      fi
    '';

  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';

  getStoreGlob = pkg: ''"${pkg.outPath}/src/**/*.purs"'';

in {
  inherit inputs;

  installSpagoStyle = pkgs.writeShellScriptBin "install-spago-style" ''
      set -e
      echo installing dependencies...
      ${builtins.toString (builtins.map cpPackage (builtins.attrValues inputs))}
      echo "echo done."
  '';

  buildSpagoStyle = pkgs.writeShellScriptBin "build-spago-style" ''
      set -e
      echo building project...
      purs compile ${builtins.toString (builtins.map getGlob (builtins.attrValues inputs))} "$@"
      echo done.
  '';

  buildFromNixStore = pkgs.writeShellScriptBin "build-from-store" ''
      set -e
      echo building project using sources from nix store...
      purs compile ${builtins.toString (
        builtins.map getStoreGlob (builtins.attrValues inputs))} "$@"
      echo done.
  '';

  mkBuildProjectOutput =
    { src, purs }:

    pkgs.stdenv.mkDerivation {
      name = "build-project-output";
      src = src;

      buildInputs = [ purs ];

      installPhase = ''
        mkdir -p $out
        purs compile "$src/**/*.purs" ${builtins.toString
          (builtins.map
            (x: ''"${x.outPath}/src/**/*.purs"'')
            (builtins.attrValues inputs))}
        mv output $out
      '';
    };
}
