# aliases
alias f "open ."
alias emacs "emacsclient -nw"

if test (uname) = "Linux"
    alias open "xdg-open"
end

# path
set -x PATH $PATH $HOME/local/bin

# rbenv
set -x PATH $PATH $HOME/.rbenv/bin/
set -x PATH $HOME/.rbenv/shims $PATH
rbenv init - | source

# pyenv
set -x PYENV_ROOT $HOME/.pyenv
set -x PATH $PYENV_ROOT/bin $PATH
set -x PATH $PYENV_ROOT/shims $PATH

# plenv
set -x PATH $HOME/.plenv/bin $PATH
plenv init - | source

# ndenv
set -x PATH $PATH $HOME/.ndenv/bin
set -x PATH $PATH $HOME/.ndenv/shims

# golang
if test (uname) = "Linux"
    set -x PATH $PATH /usr/local/go/bin
end
set -x GOPATH $HOME/develop
set -x PATH $PATH $GOPATH/bin

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
