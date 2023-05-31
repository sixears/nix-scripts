{ pkgs ? import <nixpkgs> }: pkgs.writers.writeBashBin "init-home" ''

basename=${pkgs.coreutils}/bin/basename
cmp=${pkgs.diffutils}/bin/cmp
grep=${pkgs.gnugrep}/bin/grep
ln=${pkgs.coreutils}/bin/ln
mkdir=${pkgs.coreutils}/bin/mkdir
readlink=${pkgs.coreutils}/bin/readlink
stat=${pkgs.coreutils}/bin/stat

warn () { echo "$*" 1>&2; }
die () { warn "$*"; exit 3; }

symlink() {
  local src="$1"

  if [ "x''${2:(-1)}" == "x/" -o -d "$2" ]; then
    target="''${2%/}/$($basename "$1")"
  else
    target="$2"
  fi

  if [ \! -L $target ] || [ "$( $readlink $target )" != "$src" ]; then
    echo "symlink '$src' -> '$target'"
    $ln -sf "$src" "$target" || die failed to symlink "$target"
  fi
}

md () {
  local dir="$1"
  $mkdir -p $dir || die failed to create $dir
}

symlink rc/bash/rc ~/.bash_login
symlink rc/bash/rc ~/.bashrc
symlink rc/bash/logout ~/.bash_logout

nix_defexpr=~/.nix-defexpr
md $nix_defexpr
nix_profiles=/nix/var/profiles/per-user
if [ -e $nix_profiles/root/channels ]; then
  symlink $nix_profiles/root/channels $nix_defexpr/channels_root
elif [ -e $nix_profiles/martyn/channels ]; then
  symlink $nix_profiles/martyn/channels $nix_defexpr/channels
fi

ssh_dir=~/.ssh
ssh_authkeys=$ssh_dir/authorized_keys
ssh_config=$ssh_dir/config
ssh_known_hosts=$ssh_dir/known_hosts
md $ssh_dir
for i in $ssh_config $ssh_authkeys $ssh_known_hosts; do
  symlink ../rc/ssh/"$(basename "$i")" $i
done

md ~/.config/
symlink ~/rc/systemd ~/.config/

xmonad_dir=~/.xmonad
md $xmonad_dir

symlink ../rc/xmonad/xmonad.hs $xmonad_dir/
symlink  /run/current-system/sw/bin/xsession ~/.xsession

# symlink rc/nixpkgs/ ~/.nixpkgs/

symlink rc/screen/rc ~/.screenrc
symlink rc/emacs/emacs ~/.emacs
symlink rc/haskell/ghci ~/.ghci
symlink rc/haskell/haskeline ~/.haskeline
symlink rc/git/config ~/.gitconfig

symlink rc/xscreensaver/xscreensaver ~/.xscreensaver

etc_nixos_config=/etc/nixos
git_nixos_config=/home/martyn/rc/nixos
if [ -d $etc_nixos_config ]; then
  for p in configuration.nix keys.map; do
    # quotes around readlink are necessary to ensure that if readlink returns an
    # empty string, we compare an empty string rather than lose the value
    # altogether causing an operator error (because != is a binary operator)
    src="$etc_nixos_config/$p"
    tgt="$git_nixos_config/$p"
    rl="$( $readlink "$src" )"
    # look only at the prefix of the readlink, to allow,
    # e.g., configuration.nix.nodvorakX
    [ "''${rl:0:''${#tgt}}" != "$tgt" ] && warn "$src is not symlinked to $tgt"
  done
fi

# really, this should be part of the uber-init that brings in rc in the first place
nix_home=~/rc/nixpkgs
src=~/src
src_dists=$src/dists
md $src_dists
# symlink $nix_home/dists/default.nix $src_dists/
nabal_parent=$src
nabal_home=$nabal_parent/nabal
if [ \! -d $nabal_home/.git ]; then
  cd $nabal_parent                               || die failed to cd to $nabal_parent
  git clone git@bitbucket.org:dudebout/nabal.git
  # cd ~; git clone git://github.com/nixos/nixpkgs
  # build order: textconv, fmt, fluffy
  # nabal update
  # git clone ssh://git@bitbucket.org/Mr_Fluffy/textconv.git
  # cd ~/src/textconv
  # nabal install --dependencies-only
  # nabal build
  # nabal install (to install into ~/.cabal)
  # maybe nabal-nix-install
  # git clone ssh://git@bitbucket.org/Mr_Fluffy/fluffy.git
fi
symlink $nabal_home $nix_home

# now part of os core
## # not relevant on non-wifi devices
## if $nmcli device | $grep -qs wifi; then
##   etc_nm_dispatch=/etc/NetworkManager/dispatcher.d/70-wifi-wired-exclusive.sh
##   nm_dispatch=__nm dispatch__/bin/nm-dispatch
##   [ -e $etc_nm_dispatch ]                          || die file does not exist: $etc_nm_dispatch
##   [ -f $etc_nm_dispatch ]                          || die not a regular file: $etc_nm_dispatch
##   [ $( $stat -c '%#a' $etc_nm_dispatch ) == "0544" ] \
## 						   || die file mode should be 0544: $etc_nm_dispatch
##   $cmp -s $nm_dispatch $etc_nm_dispatch            || die files differ: $nm_dispatch $etc_nm_dispatch
## fi
# that's all, folks! -----------------------------------------------------------
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
