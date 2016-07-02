set fisher_home ~/.local/share/fisherman
set fisher_config ~/.config/fisherman
source $fisher_home/config.fish

# aliases
alias ks "ls"
alias f "open ."
alias emacs "emacsclient -nw"

alias be "bundle exec"

# rbenv
set PATH $PATH $HOME/.rbenv/bin/
set PATH $HOME/.rbenv/shims $PATH

# typesafe activator
set PATH $PATH $HOME/local/bin/activator-dist-1.3.7/

# nodebrew
set PATH $HOME/.nodebrew/current/bin $PATH

# gibo
set PATH $PATH $HOME/local/bin/gibo/

# local bin
set PATH $PATH $HOME/local/bin/
