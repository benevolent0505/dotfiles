# aliases
alias ks "ls"
alias f "open ."
alias g "git"
alias emacs "emacsclient -nw"

alias be "bundle exec"

# rbenv
set PATH $PATH $HOME/.rbenv/bin/
set PATH $HOME/.rbenv/shims $PATH

# pyenv
set PYENV_ROOT $HOME/.pyenv
set PATH $PYENV_ROOT/bin $PATH

# plenv
set -x PATH $HOME/.plenv/bin $PATH
plenv init - | source

# local bin
set PATH $PATH $HOME/local/bin/

# golang
set PATH $PATH /usr/local/go/bin
set GOPATH $HOME/develop
set PATH $PATH $GOPATH/bin

function peco_change_directory
  ghq list -p | peco | read dir
  cd $dir
end

function peco_history
  history | peco | read cmd
  commandline $cmd
end

function fish_user_key_bindings
  bind \cr peco_history
  bind \cs peco_change_directory
end
