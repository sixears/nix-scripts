{ pkgs ? import <nixpkgs> {}, ... }: pkgs.writers.writeBashBin "gpg-agent-restart" ''

set -u -o pipefail
shopt -s nullglob
shopt -s dotglob

${pkgs.procps}/bin/pkill gpg-agent
${pkgs.gnupg}/bin/gpg-agent --options                                          \
             <(echo 'pinentry-program ${pkgs.pinentry}/bin/pinentry') \
             --homedir /home/martyn/.gnupg --daemon
''

# Local Variables:
# mode: sh
# End:
