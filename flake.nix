{
  description = "home-made scripts";

  inputs = {
    nixpkgs.url     = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    hpkgs1.url      = github:sixears/hpkgs1/r0.0.19.0;
#    hpkgs1.url      = path:/home/martyn/src/hpkgs1/;
    bashHeader      = {
      url    = github:sixears/bash-header/r0.0.3.0;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
#    bashHeader.url      = path:/home/martyn/src/bash-header/;
  };

  outputs = { self, nixpkgs, flake-utils, hpkgs1, bashHeader }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        bash-header = bashHeader.packages.${system}.bash-header;

        hpkgs  = hpkgs1.packages.${system};
        hlib   = hpkgs1.lib.${system};
        mkHBin = hlib.mkHBin;

        backup = mkHBin "backup" ./src/backup.hs {
          ## put this (pkgs.haskellPackages) back into hpkgs1
          libs = p: with p; with pkgs.haskellPackages;
            [
            logging-effect

            log-plus-0-0 mockio-log-0-1 parsec-plus-1-1 stdmain
            mockio-cmds-inetutils mockio-cmds-rsync mockio-cmds-util-linux
          ];
        };

#        header       = import ./src/header.nix    { inherit pkgs; };
        header       = import ./src/header.nix    { inherit pkgs; };

#        replace      = import ./src/replace.nix   { inherit pkgs; };
        ord          = import ./src/ord           { inherit pkgs; };
        execs        = import ./src/execs         { inherit pkgs; };
        dd-usb       = import ./src/dd-usb        { inherit pkgs; };
        fw           = import ./src/fw.nix        { inherit pkgs; };
        hgrep        = import ./src/hgrep.nix     { inherit pkgs; };
        nix-bell     = import ./src/nix-bell.nix  { inherit pkgs; };

        # -- environment/setup ---------

        ssh-init = import ./src/ssh-init.nix { inherit pkgs; };
        gpg-agent-restart =
          import ./src/gpg-agent-restart.nix { inherit pkgs; };

        # -- flake utilities -----------

        git-increment      = import ./src/git-increment.nix
                                    { inherit pkgs header; };
        flake-set-versions = import ./src/flake-set-versions.nix
                                    { inherit pkgs; };

        # -- music utilities -----------

        music-tag    = import ./src/music-tag
          { inherit pkgs; inherit (hpkgs) minfo; };

        mp3mk        = import ./src/mp3mk
          { inherit pkgs music-tag; };

        music = mkHBin "music" ./src/music.hs {
          libs = p: with p; with hlib.hpkgs; [
            base-unicode-symbols shake split
          ];

          replace = p: with p; {
            __coreutils__  = "${pkgs.coreutils}";
            __cdparanoia__ = "${pkgs.cdparanoia}";
            __flac__       = "${pkgs.flac}";
            __minfo__      = "${minfo}";
            __music-tag__  = "${music-tag}";
            __mp3mk__      = "${mp3mk}";
          };
        };

        # -- general utilities ---------

        queue = mkHBin "queue" ./src/queue.hs {
          libs = p: with p; [
            filelock logging-effect timers

            log-plus-0-0 mockio-log-0-1 parsec-plus-1-1 stdmain
            tasty-plus-1-5
          ];
        };

        # -- haskell utilities ---------

        bs2hs = mkHBin "bs2hs" ./src/bs2hs.hs {
          libs = p: with p; [ parsec-plus-1-1 tasty-plus-1-5 tfmt-0-2 ];
        };

      in # ---------------------------------------------------------------------

        rec {
          packages = flake-utils.lib.flattenTree {
            backup = backup.pkg;

            inherit ord execs dd-usb fw hgrep nix-bell;

            # -- environment/setup -----

            init = import ./src/init.nix
                          { inherit pkgs gpg-agent-restart ssh-init; };

            # -- flake utilities -------

            flake-meta        = import ./src/flake-meta.nix { inherit pkgs; };
            flake-out-of-date = import ./src/flake-out-of-date.nix
                                       { inherit pkgs header; };
            flake-upgrades    = import ./src/flake-upgrades.nix
                                       { inherit pkgs header flake-set-versions
                                                 git-increment; };

            # -- music utilities -------

            flacmeta     = import ./src/flacmeta      { inherit pkgs; };
            music-rename = import ./src/music-rename  { inherit pkgs; };
            tap          = import ./src/tap           { inherit pkgs; };
            mp3make      = import ./src/mp3make       { inherit pkgs mp3mk; };
            m3umv        = import ./src/m3umv
                                  { inherit pkgs execs header;
                                    music = music.pkg; };


            music = music.pkg;

            inherit music-tag mp3mk;

            # -- mkv/video utilities ---

            hb           = import ./src/hb            { inherit pkgs; };

            mkv-chapter  = (mkHBin "mkv-chapter" ./src/mkv-chapter.hs {
              libs = p: with p; [
                base-unicode-symbols data-textual lens mtl
                optparse-applicative path text

                monaderror-io more-unicode proclib stdmain
              ];

              replace = p: with p; {
                __mkvtoolnix__  = "${pkgs.mkvtoolnix}";
              };
            }).pkg;

            swap-users = import ./src/swap-users.nix { inherit pkgs; };
            battery    = import ./src/battery.nix    { inherit pkgs header; };
            queue1     = import ./src/queue1.nix     { inherit pkgs header; };

            mkv-extract-pcm = import ./src/mkv-extract-pcm.nix
                                     { inherit pkgs header; };

            vid-join  = import ./src/vid-join.nix  { inherit pkgs; };
            # inherit midentify;
            mid = (mkHBin "mid" ./src/mid.hs {
              libs = p: with p; with hlib.hpkgs; [
                base containers data-textual logging-effect mtl
                optparse-applicative text

                # we're not so much pinned to version 1, as there is a
                # duration-0.2.0.0 from hackage that we mean to avoid
                base1 duration-1-0 env-plus fpath log-plus mockio-log
                mockio-plus monadio-plus more-unicode optparse-plus stdmain
                textual-plus
              ];

              replace = p: with p; {
                __mplayer__ = "${pkgs.mplayer}";
              };
            }).pkg;

            # -- general utilities -----

            queue     = queue.pkg;
            dezip     = import ./src/dezip.nix { inherit pkgs; };
            init-home = import ./src/init-home.nix { inherit pkgs; };


            setenv  = (mkHBin "setenv" ./src/setenv.hs {
              libs = p: with p; with hlib.hpkgs; [
                range tasty-quickcheck trifecta

                env-plus stdmain tasty-plus tfmt
              ];
            }).pkg;

            # -- nix utilities ---------

            nixos-bld = import ./src/nixos-bld.nix { inherit pkgs header; };

            # -- haskell utilities -----

            bs2hs = bs2hs.pkg;
            hcomm = import ./src/hcomm.nix { inherit pkgs bash-header; };

            # -- kubernetes utilities --
            x509 = import ./src/x509.nix { inherit pkgs; };

            # -- crypt utilities -------

            crypt-mkfs    = import ./src/crypt-mkfs.nix    { inherit pkgs; };
            crypt-mount   = import ./src/crypt-mount.nix   { inherit pkgs; };
            crypt-unmount = import ./src/crypt-unmount.nix { inherit pkgs; };
          };
      }
    );
}

##
##
##  in
##    with nixpkgs;
##    rec {
##
##
##
##
##      # inherited direct from overlays
##      inherit handbrake-haskell;
##
##    }
##
