export LEIN_ROOT="TRUE"
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && exec tmux -2
