{ pkgs }: pkgs.writers.writeBashBin "hgrep" ''
exec ${pkgs.gnugrep}/bin/grep --recursive --colour      \
                              --include \*.hs           \
                              --exclude-dir .git        \
                              --exclude-dir .stversions \
                              "$@"
''
