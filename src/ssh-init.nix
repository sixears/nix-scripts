{ pkgs }: pkgs.writers.writeBashBin "ssh-init" ''

openssh=${pkgs.openssh}
ssh_agent=$openssh/bin/ssh-agent
ssh_add=$openssh/bin/ssh-add

eval $( $ssh_agent )
$ssh_add &&
  { echo writing $HOME/.ssh/env.bash;
    { echo "export SSH_AUTH_SOCK=$SSH_AUTH_SOCK";
      echo "export SSH_AGENT_PID=$SSH_AGENT_PID"; } >| $HOME/.ssh/env.bash;
    echo -e "now run\n. $HOME/.ssh/env.bash"; }
''

# Local Variables:
# mode: sh
# End:
