{ pkgs, ssh-init, gpg-agent-restart }: pkgs.writers.writeBashBin "init" ''

set -u -o pipefail
shopt -s nullglob
shopt -s dotglob

${gpg-agent-restart}/bin/gpg-agent-restart
${ssh-init}/bin/ssh-init
. ~/.ssh/env.bash
echo "adding github..." 1>&2
ssh-add ~/.ssh/github_rsa
echo "adding bitbucket..." 1>&2
ssh-add ~/.ssh/bitbucket_rsa
''

# Local Variables:
# mode: sh
# End:
