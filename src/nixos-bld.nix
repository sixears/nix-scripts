{pkgs,header}: pkgs.writers.writeBashBin "nixos-bld" ''

source ${header}

Cmd[hostname]=${pkgs.inetutils}/bin/hostname
Cmd[nixos-rebuild]=${pkgs.nixos-rebuild}/bin/nixos-rebuild

ShowTrace=false

# ------------------------------------------------------------------------------

main() {
  local impure="$1" switch="$2"

  local hostname; capture hostname gocmdnodryrun 10 hostname
  hostdir=$HOME/rc/nixos/hostcfg/hosts/$hostname
  [[ -d $hostdir ]] || die 4 "no such dir: '$hostdir'"
  go 12 cd $hostdir
  flake=$hostdir/flake.nix
  [[ -e $flake ]] || die 3 "no such flake file: '$flake'"
  local instruction=build
  local pfx_cmd=()
  if $switch; then
    instruction=switch
    pfx_cmd=( ''${Cmd[sudo]} )
  fi
  local cmd=( ''${Cmd[nixos-rebuild]} $instruction --flake $hostdir --verbose )
  $impure && cmd+=( --impure )
  $ShowTrace && cmd+=( --show-trace )
  go 11 "''${pfx_cmd[@]}" ''${Cmd[env]} -i PATH=${pkgs.git}/bin "''${cmd[@]}"
}

# -- main ----------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname [options]

build a nixos system using the host-specific flake

Options:
  -i | --impure   Allow impure evaluation
  -s | --switch   Build & switch in the result if successful
  -T | --show-trace

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
 --debug          Output additional developer debugging.
EOF
)"

getopt_args=( -o visT
              --long impure,switch,show-trace,verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

impure=false
switch=false

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -i|--impure     ) impure=true    ; shift ;;
    -s|--switch     ) switch=true    ; shift ;;
    -T|--show-trace ) ShowTrace=true ; shift ;;

    # hidden option for testing

    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true   ; shift   ;;
    --debug         ) Debug=true             ; shift ;;
    --              ) args+=("''${@:2}")     ; break ;;
    *               ) args+=("$1")           ; shift ;;
  esac
done

i=1
for x in "''${args[@]}"; do
  debug "ARG#$i: '$x'"
  i=$((i+1))
done

case ''${#args[@]} in
  0 ) main $impure $switch ;;
  * ) usage                ;;
esac

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
