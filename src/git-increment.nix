{pkgs,header}: pkgs.writers.writeBashBin "git-increment" ''

source ${header}

Cmd[git]=${pkgs.git}/bin/git
Cmd[perl]=${pkgs.perl}/bin/perl

Version=""

# ------------------------------------------------------------------------------

add_to_changelog() {
  local msg="$1" dir="$2" version="$3"

  local changelog="$dir"/changelog.md
  [[ -e $changelog ]] || { warn "no changelog found: '$changelog'"; return; }

  local date; capture date gocmdnodryrun 14 date +%Y-%m-%d
  local header="$version $date"

  local tmp; mktemp tmp --no-dry-run
  gonodryrun 27 echo "$header" >> "$tmp"
  { gonodryrun 29 printf =%.0s $(''${Cmd[seq]} 1 ''${#header}); gonodryrun 30 echo; } >> "$tmp"
  gonodryrun 28 echo "- $msg" >> "$tmp"
  gonodryrun 31 echo >> "$tmp"
  gocmdnodryrun 23 cat "$changelog" >> "$tmp"
  gocmd 15 cp $tmp "$changelog"
}

update_cabal() {
  local cabal="$1" new_version="$2"
  # local cabal=~/src/"$pkg"/"$pkg".cabal
  if [[ -e $cabal ]]; then
    gocmd 16 perl -pli -E "s[^(version:\s*)[0-9.]+$][\''${1}$new_version]" "$cabal"
  else
    warn "no .cabal found: '$cabal'"
  fi
}

update_flake() {
  local flake="$1" new_version="$2"
  if [[ -e $flake ]]; then
    gocmd 33 perl -pli -E 's/^(\s*version\s*=\s*)"[0-9.]+";/$1"'"$new_version"'";/' "$flake"
  else
    warn "no flake found: '$flake'"
  fi
}

# --------------------------------------

main() {
  local msg="$1" dirs=("''${@:2}")

# XXX optional files

  for dir in "''${dirs[@]}"; do
    gonodryrun 10 pushd "$dir" >/dev/null
    local tags;  capture tags  gocmdnodryrun 11 git tag
    local rtags
    capture rtags gocmdnodryrun 17 grep -E '^r[0-9]+(\.[0-9]+)+$' <<<"$tags"
    local tags_ ; capture tags_  gocmdnodryrun 24 sort --version-sort <<<"$rtags"
    local tagver; capture tagver gocmdnodryrun 25 tail -n 1 <<<"$tags_";
    tagver="''${tagver#r}"

    local changelog="$dir"/changelog.md
    local cl_head; capture cl_head gocmdnodryrun 12 head -n 1 "$changelog"
    local old_version
    capture old_version gocmdnodryrun 13 cut -d ' ' -f 1 <<<"$cl_head"

    local new_version
    if [[ -n ''${Version:-} ]]; then
      new_version="$Version"
    else
      if [[ $old_version != $tagver ]]; then
        die 26 "version mismatch in $dir: tags $tagver vs. changelog $old_version"
      fi

      local old_vs
      vs() {
        local IFS='.'
        old_vs=($old_version)
      }
      vs

      local new_v="''${old_vs[0]}.''${old_vs[1]}.''${old_vs[2]}"
      new_version="$new_v.$((1+''${old_vs[3]}))"
    fi

    update_flake "$PWD/flake.nix" "$new_version"
    local files=(flake.nix changelog.md)

    add_to_changelog "$msg" "$dir" "$new_version"

    local cabals=(*.cabal)

    case ''${#cabals[@]} in
      0) ;; # nothing doing
      1) update_cabal "''${cabals[0]}" "$new_version"
         files+=( "''${cabals[0]}" ) ;;
      *) die 22 "too many cabal files found: ''${cabals[*]}"
    esac

    local commit=committing
    $DryRun && commit='(would commit)'
    # pkg & space here makes c-n-p easier later
    local pkg
    capture pkg gocmdnodryrun 32 basename "$dir"
    printf "$commit %-38s  %16s  %11s ..."    "$dir" "$pkg" "$new_version"

    local flake_lock=flake.lock
    [[ -e $flake_lock ]] && files+=( "$flake_lock" )
    PATH=${pkgs.openssh}/bin:$PATH # required for git+ssh, e.g., git push
    # git --quiet is not silent
    gocmd 18 git commit --message="$msg" "''${files[@]}" >&/dev/null
    gocmd 19 git tag "r$new_version" >/dev/null
    gocmd 20 git push --tags >&/dev/null
    gonodryrun 21 popd >/dev/null
    echo "done"
  done
}

# -- main ----------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname [options] DIR+

add a changelog message, git commit flake.{nix,lock}, changelog.md, *.cabal; tag
to increment version.

E.g.,

 $Progname -m GIT-COMMIT-MESSAGE has-callstack 1.0.0.2 index 3.0.2.0

Options:
  -m | --message <MSG>     Use this message for changelog entries & git commits.
  -V | --version <VERS>    Set version to this.

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
 --debug          Output additional developer debugging.
EOF
)"

getopt_args=( -o vm:V: --long version:,message:,verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

message=""

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -m | --message )
      if [[ -z $2 ]]; then
        dieusage "argument to $1 cannot be empty"
      else
        message="$2"
        shift 2
      fi
      ;;

    -V | --version )
      if [[ $2 =~ ^[0-9]+\.[0-9]+\.[0-9]+(\.[0-9]+)?$ ]]; then
        Version=$2
      else
        dieusage "bad version: '$2'"
      fi
      shift 2
      ;;

    # hidden option for testing

    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true   ; shift   ;;
    --debug         ) Debug=true             ; shift ;;
    --              ) args+=("''${@:2}")     ; break ;;
    *               ) args+=("$1")           ; shift ;;
  esac
done

if [[ -z $message ]]; then
  dieusage "-m|--message must be provided"
fi

i=1
for x in "''${args[@]}"; do
  debug "ARG#$i: '$x'"
  i=$((i+1))
  [[ -d $x ]] || die "not a directory: '$x'"
done

case ''${#args[@]} in
  0 ) usage                          ;;
  * ) main "$message" "''${args[@]}" ;;
esac

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
