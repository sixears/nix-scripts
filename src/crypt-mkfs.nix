{pkgs?import<nixpkgs>{}}:let header=import ./header.nix{inherit pkgs;};in pkgs.writers.writeBashBin "crypt-mkfs" ''

# https://www.thegeeksearch.com/beginners-guide-to-luks-disk-encryption-in-linux/

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

Cmd[cryptsetup]=${pkgs.cryptsetup}/bin/cryptsetup
# we can't just use mkfs -t ext4, since that relies on the PATH.  Grr.
Cmd[mkfs-ext4]=${pkgs.e2fsprogs}/bin/mkfs.ext4

# ------------------------------------------------------------------------------

main() {
  local device="$1" name="$2"

  exec_as_root; check $? exec_as_root

  local mapper_device="/dev/mapper/$name"

  # create a new encrypted partition
  gocmd 10 cryptsetup luksFormat "$device"

  # access the new partition
  gocmd 11 cryptsetup luksOpen "$device" "$name"

  # make a filesystem on the new partition
  gocmd 12 mkfs-ext4 -t "$mapper_device"

  # undo the cryptsetup, to allow a later call to crypt-mount
  gocmd 13 cryptsetup luksClose "$name"

  echo "Created an ext4 filesystem on '$device'"
}

# ------------------------------------------------------------------------------

getopt_args=( -o v --long verbose,debug,dry-run,help )
OPTS=$( ''${Cmd[getopt]} ''${getopt_args[@]} -n "$Progname" -- "$@" )

[ $? -eq 0 ] || dieusage "options parsing failed (--help for help)"

name=usb-create

debug "OPTS: '$OPTS'"
# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

while true; do
  case "$1" in
    # don't forget to update $Usage!!
    -n | --name                       ) name="$2"       ; shift 2 ;;

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

Create a LUKS-encrypted filesystem on some device

Options:

  -n | --name  <NAME>      Name of the device-mapper device to use.
                           This doesn't normally matter, as it is only used
                           during the formatting phase; but it needs to not
                           clash with any in-use name.
                           Default: '$name'

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
  1 ) main "''${args[0]}" "$name" ;;
  * ) usage                       ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
