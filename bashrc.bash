PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && exec tmux -2
