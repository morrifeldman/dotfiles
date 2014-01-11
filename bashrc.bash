export LEIN_ROOT="TRUE"
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ $- != *i* ]] && return
#[[ -z "$TMUX" ]] && (tmux -2 attach || tmux -2 new-session)
echo "Starting TMUX"
[[ -z "$TMUX" ]] && exec tmux -2
