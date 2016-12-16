# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# node
export PATH="$HOME/.ndenv/bin:$PATH"
eval "$(ndenv init -)"

# plenv
export PATH="$HOME/.plenv/bin:$PATH"
eval "$(plenv init -)"

# golang
export GOPATH="$HOME/develop"
export PATH="$PATH:$GOPATH/bin"
