{pkgs?import<nixpkgs>{}}:let header=import ./header.nix{inherit pkgs;};in pkgs.writers.writeBashBin "crypt-mount" ''

# https://www.thegeeksearch.com/beginners-guide-to-luks-disk-encryption-in-linux/

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

Cmd[cryptsetup]=${pkgs.cryptsetup}/bin/cryptsetup
Cmd[mount]=/run/current-system/sw/bin/mount

# ------------------------------------------------------------------------------

main() {
  local device="$1" name="$2" mountpoint="$3"

  exec_as_root; check $? exec_as_root

  local mapper_device="/dev/mapper/$name"

  # access the new partition
  gocmd 10 cryptsetup luksOpen "$device" "$name"

  # mount the filesystem
  gocmd 11 mount "$mapper_device" "$mountpoint"

  echo "Mounted '$mapper_device' at '$mountpoint'"
}

# ------------------------------------------------------------------------------

getopt_args=( -o v --long verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

name=usb
mountpoint=/mnt/usb

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -n | --name                         ) name="$2"       ; shift 2 ;;
    -m | --mnt | --mount | --mountpoint ) mountpoint="$2" ; shift 2 ;;

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
Usage: $Progname [options] <DEVICE>

Mount a LUKS-encrypted device

Options:

  -n | --name  <NAME>        Name of the device-mapper device to use.
                             Default: '$name'
  -m | --mnt   <MOUNTPOINT>  Name of the device-mapper device to use.
  --mount | --mountpoint     Default: 'mountpoint'

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
  1 ) main "''${args[@]}" "$name" "$mountpoint" ;;
  * ) usage                                     ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
