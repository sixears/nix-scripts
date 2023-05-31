{pkgs,header,flake-set-versions,git-increment}: pkgs.writers.writeBashBin "flake-upgrades" ''

source ${header}

declare tempdir

mktemp tempdir --dir --no-dry-run

declare -A new_pkg_version=() input_pkg_version=()

Cmd[flake-set-versions]=${flake-set-versions}/bin/flake-set-versions
Cmd[git]=${pkgs.git}/bin/git
Cmd[git-increment]=${git-increment}/bin/git-increment
Cmd[perl]=${pkgs.perl}/bin/perl

# ------------------------------------------------------------------------------

set_versions() {
  local pkg="$1"
  local orig_flake=~/src/"$1"/flake.nix
  local flake="$orig_flake"
  local tmpflake="$tempdir/$pkg.nix"
  [[ -e $tmpflake ]] && flake="$tmpflake"

  local x
  local new_pkg_versions="$(for x in "''${!new_pkg_version[@]}"; do
                              echo -n $x "''${new_pkg_version[$x]}" ' '
                            done)"

  debug "new_pkg_versions: >>$new_pkg_versions<< ($pkg)"

  local tmp
  mktemp tmp --no-dry-run
  if gocmdnoexitnodryrun flake-set-versions "$flake" $new_pkg_versions >| "$tmp"
  then
    gocmdnodryrun 10 cp "$tmp" "$tmpflake"
    gocmd         17 cp "$tmpflake" "$orig_flake"
    return 0
  else
    return 1
  fi
}

# --------------------------------------

# read the latest version from the changelog
changelog_v() {
  local __varname="$1" __changelog="$2"
  local __cl_head; capture __cl_head gocmdnodryrun 12 head -n 1 "$__changelog"
  local __old_version
  capture __old_version gocmdnodryrun 13 cut -d ' ' -f 1 <<<"$__cl_head"
  debug "changelog version $__old_version for $__changelog ($__varname)"
  printf -v "$__varname" %s "$__old_version"
}

# --------------------------------------

upgrade_pkg() {
  local message="$1" pkg="$2"
  if set_versions "$pkg"; then
    if [[ x != ''${new_pkg_version[$pkg]:-x} ]]; then
      # we've already seen this, and updated the version; so let's not do
      # that again
      return 0
    fi
  else
    # no update was made
    return 1
  fi

  # add a message to the changelog
  local old_version
  local changelog=~/src/"$pkg"/changelog.md
  changelog_v old_version "$changelog"
  if ! [[ ''${old_version:-} =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    die 24 "no old version found for pkg '$pkg' ($changelog) [$old_version]"
  fi
  debug "old_version: $old_version"

  local old_vs
  vs() {
    local IFS='.'
    old_vs=($old_version)
  }
  vs
  local new_v="''${old_vs[0]}.''${old_vs[1]}.''${old_vs[2]}"
  local new_version="$new_v.$((1+''${old_vs[3]}))"

  new_pkg_version[$pkg]=$new_version
  return 0
}

# --------------------------------------

prepare_upgrades() {
  local message="$1"

  local changed=true
  while $changed; do
    changed=false
    local f; for f in ~/src/*/flake.nix; do
      if [[ $f =~ /src/([a-z0-9-]+)/flake.nix$ ]]; then
        local p="''${BASH_REMATCH[1]}"
        upgrade_pkg "$message" "$p" && changed=true
      else
        dieinternal "unrecognized flake: '$f'"
      fi
    done
  done
}

# --------------------------------------

git_commits() {
  local pkg
  for pkg in "''${!new_pkg_version[@]}"; do
    # don't git commit input pkgs; they should already be committed
    [[ x != ''${input_pkg_version[$pkg]:-x} ]] && continue

    local args=( --version "''${new_pkg_version[$pkg]}"
                 --message "$message" )
    $DryRun && args+=( --dry-run )
    gocmdnodryrun 22 git-increment "''${args[@]}" ~/src/"$pkg"
  done
}

# -- main ----------------------------------------------------------------------

getopt_args=( -o vm:M
              --long default-message,message:,verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

message=""
default_message=false

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
    -M | --default-message ) default_message=true ; shift ;;

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
Usage: $Progname [options] (package=version)+

Upgrade all required flakes in ~/src/\* to use some given versions.

E.g.,

 $Progname -m GIT-COMMIT-MSG has-callstack 1.0.0.2 index 3.0.2.0

Options:
  -m | --message <MSG>     Use this message for changelog entries & git commits.
  -M | --default-message   Derive a default message from the upgrades

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
  0 ) dieusage "please provide some package/version pairs" ;;
  * ) for i in "''${args[@]}"; do
        if [[ $i =~ ^([-a-z0-9]+)=r?([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)$ ]]; then
          p="''${BASH_REMATCH[1]}"
          v="''${BASH_REMATCH[2]}"
          # Ugh.  Bug(?) in bash-5.1.16 treats $p as an unset var
          # if it doesn't already exist in new_pkg_version
          # set +u;
          new_pkg_version[$p]="$v"; input_pkg_version[$p]="$v";
          # set -u
        elif [[ . == ''${args[0]} ]]; then
          # special case; imply -M if '.' is the only argument and
          # no -m is provided
          if [[ 1 -eq ''${#args[@]} ]] && [[ -z $message ]]; then
            default_message=true
          fi
          if [[ -e changelog.md ]]; then
            capture p gocmdnodryrun 11 basename "$PWD"
            changelog_v v changelog.md
            debug "p: '$p' ($PWD)	v: '$v'"
            new_pkg_version[$p]="$v"
            input_pkg_version[$p]="$v"
          else
            dieusage "cannot use '.' in flake-less dir".
          fi
        else
          dieusage "failed to parse pkg=version '$i'"
        fi
      done
      ;;
esac

if $default_message; then
  if [[ -n $message ]]; then
    dieusage "cannot combine default-message with message '$message'"
  else
    msgs=()
    for k in "''${!new_pkg_version[@]}"; do
      msgs+=( "$k->''${new_pkg_version[$k]}" )
    done
    old_IFS="$IFS"
    IFS=\;
    message="''${msgs[*]}"
    IFS="$old_IFS"
  fi
fi

if [[ -z $message ]]; then
  dieusage "-m|--message must be provided"
fi

prepare_upgrades "$message"
git_commits "$message"

# -- that's all, folks! --------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
