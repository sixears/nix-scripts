{pkgs?import<pkgs>{},header?import ./header.nix{inherit pkgs;}}: pkgs.writers.writeBashBin "mkv-extract-pcm" ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

Cmd[flac]=${pkgs.flac}/bin/flac
Cmd[mkvextract]=${pkgs.mkvtoolnix}/bin/mkvextract

Recode=false

# ------------------------------------------------------------------------------

main() {
  trackid="$1" input="$2" output="''${3%.flac}".flac

  [[ -e $input ]] || dieusage "No such input file '$input'"
  [[ $trackid =~ ^[0-9]+$ ]] || dieusage "TrackID not an integer ($trackid)"
  [[ -e $output ]] && dieusage "not overwriting extant '$output'"

  local dir
  capture dir gocmdnodryrun 10 dirname "$input"
  local tmpwav
  mktemp --tmpdir "$dir" --suffix .wav --no-dry-run tmpwav
  if $Recode; then
    local tmpflac
    mktemp --tmpdir "$dir" --suffix .flac --no-dry-run tmpflac
    gocmd 11 mkvextract "$input" tracks "$trackid":"$tmpflac"
    gocmd 12 flac -d "$tmpflac" --output-name "$tmpwav" --force
  else
    gocmd 13 mkvextract "$input" tracks "$trackid":"$tmpwav"
  fi

  gocmd 14 flac --best "$tmpwav" --output-name="$output"
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} << EOF
usage: $Progname OPTIONS* TRACKID INPUT-MKV OUTPUT-FLAC

Extract PCM from MKV, encode to FLAC.
Note that the trackid is that passed to mkvextract.
The output file name is inferred from the input filename, replacing .mkv with
.flac (but see -o).
Existing files are not overwritten.

Options:

  -o | --output   Output to this file.  Only valid if a single input file is
                  provided.
  -r | --recode   Audio is presumed to be in flac (in the input file); but
                  is recoded to ensure best compression and details like stream
                  length are included in the file.

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

getopt_args=( -o vo:r
              --long output:,recode,verbose,dry-run,help
              -n "$Progname" -- "$@"
            )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()

output=""
while true; do
  case "$1" in
    -o | --output   ) output="$2" ; shift 2 ;;
    -r | --recode   ) Recode=true ; shift   ;;
    # don't forget to update $Usage!!

    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true            ; shift   ;;
    --              ) shift; args+=( "$@" )  ; break   ;;
    *               ) args+=( "$1" )         ; shift   ;;
  esac
done

case ''${#args[@]} in
  0) usage ;;
  1) usage ;;
  2) main "''${args[@]}" "''${output:-''${args[1]%.mkv}.flac}" ;;
  *) if [[ -n $output ]]; then
       dieusage "cannot use --output with multiple input files"
     else
       trackid="''${args[0]}"
       for arg in "''${args[@]:1}"; do
         main "$trackid" "$arg" "''${arg%.mkv}.flac"
       done
     fi
     ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:

# ------------------------------------------------------------------------------
