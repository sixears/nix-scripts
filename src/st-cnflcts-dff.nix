{pkgs,header}: pkgs.writers.writeBashBin "st-cnflcts-dff" ''
# find all the sync-conflicts relating to a file, diff them (if there are only
# two files, by md5sum), give the user the option to clean up

source ${header}

basename=${pkgs.coreutils}/bin/basename
clear=${pkgs.ncurses}/bin/clear
cut=${pkgs.coreutils}/bin/cut
diff=${pkgs.diffutils}/bin/diff
dirname=${pkgs.coreutils}/bin/dirname
fd=${pkgs.fd}/bin/fd
grep=${pkgs.gnugrep}/bin/grep
head=${pkgs.coreutils}/bin/head
md5sum=${pkgs.coreutils}/bin/md5sum
mv=${pkgs.coreutils}/bin/mv
tail=${pkgs.coreutils}/bin/tail
rm=${pkgs.coreutils}/bin/rm
seq=${pkgs.coreutils}/bin/seq

# this pattern is used both as a bash (=~) pattern and an fd pattern
readonly st_re='\.sync-conflict-(19|20)[0-9]{6}-[0-9]{6}-[A-Z0-9]{7}(\.[^./]+)?$'

# --------------------------------------

# remove duplicates from an array
dedup() {
  local -n varname="$1"

  local -A seen=()
  local -a new=()
  local i
  for i in "''${varname[@]}"; do
    if [[ -z ''${seen[$i]:-} ]]; then
      new+=( "$i" )
      seen[$i]=yes
    fi
  done

  varname=( "''${new[@]}" )
}

# --------------------------------------

# filter an array only strings matching a pattern remain
filter() {
  local -n varname="$1"
  local pat="$2"

  local -a new=()
  local i
  for i in "''${varname[@]}"; do
    if [[ $i =~ $pat ]]; then
      new+=( "$i" )
    fi
  done

  varname=( "''${new[@]}" )
}

# --------------------------------------

# remove a string from an array var
remove_str() {
  local -n varname="$1"
  local str="$2"

  local -a new=()
  local i
  for i in "''${varname[@]}"; do
    if [[ $i != "$str" ]]; then
      new+=( "$i" )
    fi
  done

  varname=( "''${new[@]}" )
}

# --------------------------------------

# rename a var if it looks like a sync-conflict file to its root file
de_st() {
  local -n varname="$1"
  if [[ $varname =~ ^(.*)$st_re ]]; then
    varname="''${BASH_REMATCH[1]}''${BASH_REMATCH[3]}"
  fi
}

# --------------------------------------

# rename each element of an array with de_st
de_sts() {
  local -n varname="$1"

  local i
  for i in $( $seq 0 $((''${#varname[@]}-1)) ); do
    local x="''${varname[$i]}"
    de_st x
    varname[$i]="$x"
  done
}

# --------------------------------------

main() {
  local f="$1"
  de_st f
  # sanity check that f is real, and a regular file
  [[ -e $f ]] || die 13 "no such file '$f'"
  [[ -f $f ]] || die 14 "not a regular file '$f'"

  local base="$($basename "$f")"
  local dir="$($dirname "$f")"
  local stub="''${base%.*}"
  local ext=""
  [[ $base =~ \. ]] && ext="''${base##*.}"
  # sync-conflict files corresponding to f
  local sync_conflict_files=( "$dir/$stub.sync-conflict-"*"''${ext:+.$ext}" )
  if [[ ''${#sync_conflict_files[@]} -eq 0 ]]; then
    die 1 "no conflict found found for '$dir/$stub.sync-conflict-*''${ext:+.$ext}'"
  fi

  # md5 sum of f
  local md5_f
  # map from md5 to newline-separated list of files with that md5
  local -A md5

  md5file() {
    local i="$1"
    local mx="$($md5sum "$i" | $cut -c 1-32)"
    [[ $i == $f ]] && md5_f="$mx"
    if [[ -z ''${md5[$mx]:-} ]]; then
      md5[$mx]="$i"
    else
      md5[$mx]+=$'\n'"$i"
    fi
  }
  md5files() { for i in "$@"; do md5file "$i"; done; }

  md5files "$f" "''${sync_conflict_files[@]}"

  case ''${#md5[@]} in
    0 ) echo "no MD5s found! '$f'"
        ;;

    1 ) echo "no diffs found in ''${md5[@]}"

        local -a files_like_f
        readarray -t files_like_f <<<"''${md5[@]}"

        ## remove other files equal to f
        remove_str files_like_f "$f"
        filter files_like_f "$st_re"

        local j
        for j in "''${files_like_f[@]}"; do
          $rm -v "$j"
        done
        ;;

    2 ) local md5s=( "''${!md5[@]}" )
        # each a single string with newlines between filenames
        local -a files_like_f files_like_other
        if [[ ''${md5s[0]} == "$md5_f" ]]; then
          readarray -t files_like_f <<<"''${md5[''${md5s[0]}]}"
          readarray -t files_like_other <<<"''${md5[''${md5s[1]}]}"
        else
          readarray -t files_like_f <<<"''${md5[''${md5s[1]}]}"
          readarray -t files_like_other <<<"''${md5[''${md5s[0]}]}"
        fi

        ## remove other files equal to f
        remove_str files_like_f "$f"
        filter files_like_f "$st_re"

        local j
        for j in "''${files_like_f[@]}"; do
          echo "($rm $j)"
        done

        local other="''${files_like_other[0]}"

        echo $diff -y "$f" "$other" || die 18 "diff failed"
        echo ---
        $diff -y "$f" "$other"
        echo
        read -p "select l(eft)|r(ight)|n(one)> " reply
        case "''${reply,,}" in
          ## remove ALL the other files,
          l | left )
            for j in "''${files_like_other[@]}"; do $rm -v "$j"; done
            ;;
          ## move one to $f and remove the rest
          r | right )
            $mv -v "$other" "$f"
            for j in "''${files_like_other[@]:1}"; do
              $rm -v "$j"
            done
            ;;
          n | none ) : ;;
          * ) die 17 WRONG
        esac
        ;;

    * ) echo "too many MD5s found:"

        for m in "''${!md5[@]}"; do
          echo "$m"
          while read a; do
            echo "  $a"
          done <<<"''${md5[$m]}"
        done
        ;;
  esac
}

Usage="$(''${Cmd[cat]} <<EOF
Usage: $Progname [options]

diff sync-conflict files, clean them up

Options:
  -a | --all      Look through all conflict files

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
 --debug          Output additional developer debugging.
EOF
)"

getopt_args=( -o va
              --long all,verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

do_all=false

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -a|--all ) do_all=true ; shift ;;

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

if $do_all; then
  if [[ ''${#args[@]} != 0 ]]; then
    die 10 "cannot mix -a|-all with arguments"
  else
    readarray -t args < <($fd --hidden "$st_re" ~)
    de_sts args
    dedup args
  fi
fi

for f in "''${args[@]}"; do
  $clear
  main "$f"
done

''

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
