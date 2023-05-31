{pkgs,header}: pkgs.writers.writeBashBin "flake-out-of-date" ''

source ${header}

# ------------------------------------------------------------------------------

main() {
  local i

  local -A vs=()
  for i in ~/src/*/flake.nix; do
    [[ $i =~ /src/([a-z0-9-]+)/flake.nix ]]
    local p=''${BASH_REMATCH[1]}
    local changelog=~/src/$p/changelog.md
    if [[ -f $changelog ]]; then
      local v v_
      capture v_ gocmd 11 head -n 1 $changelog
      capture v gocmd 12 cut -d ' ' -f 1 <<<"$v_"
      vs[$p]=$v
    fi
  done

  for i in ~/src/*/flake.nix; do
    [[ $i =~ /src/([a-z0-9-]+)/flake.nix ]]
    local p=''${BASH_REMATCH[1]}
    while read j; do
      if [[ $j =~ ^\ *([a-z0-9-]+).url\ *=\ *\"?github:.*/([^/]+)/([^/\"]+?)\"?\;.*$ ]]
      then
        pkg=''${BASH_REMATCH[2]}
        ver=''${BASH_REMATCH[3]}
        if [[ x != ''${vs[$pkg]:-x} ]] && [[ r''${vs[$pkg]} != $ver ]]; then
          printf '%-32s  %11s  %13s  %-32s  %s\n' \
                 "$pkg" "''${vs[$pkg]}" "($ver)" "$p" "$i"
        fi;
      else
        echo "no match '$j' ($i)";
        break;
      fi
    done < <(gocmd 10 grep .url $i);
  done
}

# -- main ----------------------------------------------------------------------

getopt_args=( -o vm: --long message:,verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

while true; do
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
Usage: $Progname [options]

Show out-of-date builds cited in current flakes

Options:

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
  0 ) main ;;
  * ) usage ;;
esac

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
