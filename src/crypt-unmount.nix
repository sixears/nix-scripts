{pkgs?import<nixpkgs>{}}:let header=import ./header.nix{inherit pkgs;};in pkgs.writers.writeBashBin "crypt-unmount" ''

# https://www.thegeeksearch.com/beginners-guide-to-luks-disk-encryption-in-linux/

set -u -o pipefail -o noclobber; shopt -s nullglob
PATH=/dev/null

source ${header}

Cmd[cryptsetup]=${pkgs.cryptsetup}/bin/cryptsetup
Cmd[umount]=/run/current-system/sw/bin/umount

# ------------------------------------------------------------------------------

main() {
  local name="$1" mountpoint="$2"

  exec_as_root; check $? exec_as_root

  local mapper_device="/dev/mapper/$name"

  # unmount the filesystem
  gocmd 10 umount "$mountpoint"
  gocmd 11 cryptsetup luksClose "$name"

  echo "Unmounted '$mapper_device' from '$mountpoint'"
}

# ------------------------------------------------------------------------------

getopt_args=( -o v --long verbose,debug,dry-run,help )
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
Usage: $Progname [options] [NAME] [MOUNTPOINT]

Unmount a LUKS-encrypted device.

The NAME defaults to 'usb'; the MOUNTPOINT to /mnt/\$name .

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
  0 ) main usb /mnt/usb                       ;;
  1 ) main "''${args[0]}" "/mnt/''${args[0]}" ;;
  2 ) main "''${args[@]}"                     ;;
  * ) usage                                   ;;
esac

# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
