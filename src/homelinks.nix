{ pkgs }: pkgs.writers.writeBashBin "homelinks" ''

# check that the symlinks specified in ~/rc/homelinks/homelinks
# are in place

PATH=

basename=${pkgs.coreutils}/bin/basename
find=${pkgs.findutils}/bin/find
grep=${pkgs.gnugrep}/bin/grep
hostname=${pkgs.nettools}/bin/hostname
readlink=${pkgs.coreutils}/bin/readlink

set -eu -o pipefail
shopt -s nullglob

cd ~

warn() { echo "$@" >&2; }

check() {
  local source="$1" target="$2"

  if [[ -e $target ]] || [[ -L $target ]]; then
    if [[ -L $target ]]; then
      local link
      link="$($readlink "$target" || true)"
      if [[ $source != $link ]]; then
        warn "$target should point to $source (got '$link')"
      fi
    else
      warn "$target is not a symlink (should point to $source)"
    fi
  else
    warn "$target does not exist (should point to $source)"
  fi
}

while read target source; do
  if [[ $source =~ \* ]]; then
    if [[ -n "$(echo "$HOME"/$source)" ]]; then
      while read i; do
        b="$($basename "$i")"
        check "$i" "''${target%/}/$b"
      done < <( $find "$HOME"/$source -follow -type f )
    fi
  else
    check "$source" "$target"
  fi
done < <( /run/current-system/sw/bin/grep -Ev '^ *(#.*)?$' ~/rc/homelinks/homelinks )

hostname="$($hostname -s)"
homelinks_touch=$HOME/.homelinks.$hostname.touch
touch $HOME/.homelinks.$hostname.touch
''

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
