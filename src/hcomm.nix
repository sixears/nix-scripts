{ pkgs, bash-header }: pkgs.writers.writeBashBin "hcomm" ''

source ${bash-header}

Cmd[git]=${pkgs.git}/bin/git

shopt -s extglob  # for ls glob +(...); also rm !(...).nix
shopt -u nullglob # so ls on no files gives no answer

# -- main ----------------------------------------------------------------------

main () {
  local version
  capture version gocmdnodryrun 10 head --lines 1 changelog.md
  capture version gocmdnodryrun 11 cut -d ' ' -f 1 <<<"$version"
  if ! [[ $version =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    die_unless_dryrun 12 "bad version: '$version'"
  fi

  local package
  capture package gonodryrun 13 pwd
  capture package gocmdnodryrun 14 basename "$package"

  local pcount
  capture pcount gocmdnodryrun 15 ls -1                                        \
          ~/src/dists/"$package"-+([0-9])\.+([0-9])\.+([0-9])\.+([0-9]).tar.gz \
          2>/dev/null
  capture pcount gocmdnodryrun 16 wc -l <<<"$pcount"

  [[ 0 -eq $pcount ]] && die_unless_dryrun 17 "unknown package: '$package'"

  targz=~/src/$package/dist-newstyle/sdist/"$package-$version".tar.gz
  [[ -e $targz ]] || die_unless_dryrun 18 "does not exist: '$targz'"
  echo "$package-$version ($targz)"

  local tags
  capture_array tags gocmdnodryrun 19 git tag
  gocmdnoexitnodryrun grep --no-messages --silent "r$version" <<< "''${tags[@]}"
  local rv=$?
  [[ 0 -eq $rv ]] && die_unless_dryrun 20 "tag 'r$version' already exists"

  local status
  capture status gocmdnodryrun 21 git status --short
  gocmdnoexitnodryrun grep --no-messages --silent '^??' <<< "$status"
  local unclean
  capture_array unclean gocmdnodryrun 22 cut -c 4- <<<"$status"
  if [[ 0 -eq $rv ]]; then
    die_unless_dryrun 22 "unclean files exist (''${unclean[*]})"
  fi

  warnf "committing r$version..."; gocmd 23 git commit -m "r$version" -a; echo
  warnf "tagging r$version..."; gocmd 24 git tag "r$version"; echo
  warnf "pushing tags..."; gocmd 25 git push --tags; echo

  local dists=~/src/dists
  cd ~/src/dists
  for i in $dists/$package-!($version).tar.gz; do
    warnf "Moving $i to the Attic..."; gocmd 26 mv "$i" $dists/Attic/; echo
  done

  echo "now add it to hpkgs using ~/bin/cabal2callPkg to generate the stanzas"
}

# ----------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname [options]

Commit a haskell package, including tagging.

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
  0 ) main  ;;
  * ) usage ;;
esac

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
