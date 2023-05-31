# -*- mode: shell-script -*-
{ pkgs, ... }: pkgs.writers.writeBashBin "nix-bell" ''

# -u: Treat unset variables and parameters other than the special parameters "@"
#     and "*" as an error when performing parameter expansion.  If expansion is
#     attempted on an unset variable or parameter, the shell prints an error
#     message, and, if not interactive, exits with a non-zero status.

# -o pipefail: If set, the return value of a pipeline is the value of the last
#              (rightmost) command to exit with a non-zero status, or zero if
#              all commands in the pipeline exit successfully.  This option is
#              disabled by default.

builtin set -u -o pipefail

# nullglob: If set, bash allows patterns which match no files to expand to a
#           null string, rather than themselves.
# dotglob:  If set, bash includes filenames beginning with a . in the results of
#           pathname expansion.
builtin shopt -s nullglob
builtin shopt -s dotglob

basename=${pkgs.coreutils}/bin/basename
false=${pkgs.coreutils}/bin/false
getopt=${pkgs.utillinux}/bin/getopt
true=${pkgs.coreutils}/bin/true

progname="$($basename "$0")"
verbose=$false
dry_run=$false

cat=${pkgs.coreutils}/bin/cat
mktemp=${pkgs.coreutils}/bin/mktemp
rm=${pkgs.coreutils}/bin/rm

# ------------------------------------------------------------------------------

warn () {
  echo -e "$1" 1>&2
}

info () {
  if $verbose; then
    echo -e "$1" 1>&2
  fi
}

usage () {
  usage="$($cat <<EOF
usage: $progname OPTION*

run nix-shell, with \$out set to a temporary directory

options:
 -a | --artist  <ARTIST>  set album artist
 -u | --user    <USER>    add a link to the user's area (may be repeated)
 -V | --various           set album artist to 'Various Artists'

 -v | --verbose
 -n | --dry-run
 -h | --help
EOF
)"
  die 2 "$usage"
}

die() {
  warn "$2"
  builtin exit $1
}

go() {
  exit="$1"; shift
  if $dry_run; then info "(CMD) $*"; else info "CMD> $*"; fi
  $dry_run || eval "$@" || die "$exit" "failed: $*"
}

# ------------------------------------------------------------------------------

OPTS=$( $getopt -o vhnVa:u: --long verbose,dry-run,help,various,user:,artist: \
                -n "$progname" -- "$@" )

[ $? -eq 0 ] || die 2 "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()
while true; do
  case "$1" in
    # !!! don't forget to update usage !!!
    -v | --verbose  ) verbose=$true            ; shift   ;;
    -h | --help     ) usage                              ;;
    -n | --dry-run  ) dry_run=$true            ; shift   ;;
    --              ) args+=("''${@:2}"); break ;;
    *               ) args+=("$1"); shift     ;;
  esac
done

[ $# -eq 0 ] && usage

tmpdir="$($mktemp -d -t "$progname.XXX")"
builtin trap "\"$rm\" -fr \"$tmpdir\"" EXIT
inst=$tmpdir/install
${pkgs.nix}/bin/nix-shell --command "export out=$inst prefix=$inst; return" \
                                    "''${args[@]}"
''
