{pkgs?import<nixpkgs>{},header?import ./header.nix{inherit pkgs;}}: pkgs.writers.writeBashBin "vid-join" ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

Cmd[ffmpeg]=${pkgs.ffmpeg}/bin/ffmpeg

# ------------------------------------------------------------------------------

main() {
  local out="$1" ins=("''${@:2}")
  local startdir="$PWD"
  local tempdir

  debug "output: '$out'"
  [[ -e $out ]] && die 15 "not overwriting extant '$out'"
  local i
  for i in "''${ins[@]}"; do
    debug "input: '$i'"
    [[ -e $i ]] || die 16 "no such input: '$i'"
  done

  mktemp --dir --tmpdir "$startdir" tempdir

  local i=001
  for i in "''${ins[@]}"; do
    gocmd 11 ln -s "../$i" "$tempdir/$i"
    echo "file '$i'" >> "$tempdir/inputs.txt"
    i="$(printf %03d $((i+1)))"
  done
  gocmd 12 cat "$tempdir/inputs.txt"
  gocmd 13 ls -l "$tempdir"
  go 10 cd "$tempdir"
  [[ ${out:0:1} == / ]] || out="$startdir/$out"
  gocmd 14 ffmpeg -f concat -i "$tempdir/inputs.txt" -c copy "$out"
}

# ------------------------------------------------------------------------------

getopt_args=( -o v --long verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

while true; do
  debug "processing arg '$1'"
  case "$1" in
    # don't forget to update $Usage!!

    # hidden option for testing

    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true   ; shift   ;;
    --debug         ) Debug=true             ; shift ;;
    --              ) args+=("''${@:2}")     ; break ;;
    *               ) args+=("$1")           ; shift ;;
  esac
done

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname

join several videos together

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
 --debug          Output additional developer debugging.
EOF
)"

i=1
for x in "''${args[@]}"; do
  debug "ARG#$i: '$x'"
  i=$((i+1))
done

case ''${#args[@]} in
  0 ) usage               ;;
  1 ) usage               ;;
  2 ) usage               ;;
  * ) main "''${args[@]}" ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
