{ pkgs, header ? import ./header.nix { inherit pkgs; } }: pkgs.writers.writeBashBin "queue1" ''

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

# ------------------------------------------------------------------------------

# Grab the (f)lock(s) to execute the command.
#
# Grab the queue flock or die.
# Wait for the exec flock.  Once we have that, release the queue flock for the
# next instance to queue up on.  Then, execute.
# ARGS
#   queue_flock ) file to flock to own the queue (of 1)
#   exec_flock  ) file to flock to own the execution
#   cmd+        ) the cmd to execute
main() {
  local queue_flock="$1" exec_flock="$2" cmd=("''${@:3}")

  goeval 16 exec 4\<\> "$queue_flock"

  if ! gocmdnoexit flock --exclusive --nonblock 4; then
    pid="$(gocmd 12 cat $queue_flock)"; check_ "cat $queue_flock"
    # there's one already queued is /OK/, so we exit 0
    die 0 "cannot flock '$queue_flock': pid <$pid> is already queued"
  fi

  # We have the flock on $queue_flock.
  # Write this with a separate redirection, to ensure a truncating overwrite.
  goeval 14 echo $$ \>\| "$queue_flock"

  # We're queued, so we just wait for the exec lock.
  goeval 15 exec 5\<\> "$exec_flock"
  gocmd 13 flock --exclusive 5

  # We have the flock on $exec_flock.
  # Write this with a separate redirection, to ensure a truncating overwrite.
  goeval 17 echo $$ \>\| "$exec_flock"
  gocmd 18 flock --unlock 4

  gonoexit "''${cmd[@]}"; local rv=$?
  gocmdeval 19 cat /dev/null \>\| "$exec_flock"

  gonoexit exit $rv
}

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} << EOF
usage: $Progname OPTION* CMD+

Perform an action with a flock to prevent duplication; allow for a single
instance to queue up to act after this one.  If there is already a queue, abort.

Options:
  --queue-flock|-q  Use this file as the queue flock.
  --exec-flock|-x   Use this file as the exec flock.

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

queue_flock=""
exec_flock=""

getopt_args=( -o vq:x: --long queue-flock:,exec-flock:,verbose,dry-run,help
              -n "$Progname" -- "$@" )
OPTS=$( ''${Cmd[getopt]} "''${getopt_args[@]}" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()

while true; do
  case "$1" in
    -x | --queue-flock  ) queue_flock="$2" ; shift 2 ;;
    -q | --exec-flock   ) exec_flock="$2"  ; shift 2 ;;
    # don't forget to update $Usage!!

    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true            ; shift   ;;
    --              ) shift; args+=( "$@" )  ; break   ;;
    *               ) args+=( "$1" )         ; shift   ;;
  esac
done

if [[ 0 -eq ''${#args[@]} ]]; then
  usage
fi

[[ ''${args[0]} =~ /$ ]] && dieusage "arg 0 may not end with '/' (''${args[0]})"

if [[ -z $queue_flock ]]; then
  queue_flock=''${TMPDIR:-/tmp''${USER:+/$USER}}/''${args[0]##*/}.queue
fi

if [[ -z $exec_flock ]]; then
  exec_flock=''${TMPDIR:-/tmp''${USER:+/$USER}}/''${args[0]##*/}.exec
fi

real_queue_flock="$( gocmd 10 realpath "$queue_flock" )";
check_ "realpath $queue_flock"
real_exec_flock="$( gocmd 11 realpath "$exec_flock" )";
check_ "realpath $exec_flock"

if [[ $real_exec_flock == $real_queue_flock ]]; then
  local msg="exec flock ($exec_flock) & queue flock ($queue_flock) both resolve"
  msg+=" to $real_exec_flock!"
  dieusage "$msg"
fi

main "$queue_flock" "$exec_flock" "''${args[@]}"
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:

