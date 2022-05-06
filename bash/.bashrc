# PATHの設定

# Golang
export GOPATH="$HOME/develop"
export PATH="$PATH:$GOPATH/bin"

# Node
eval "$(nodenv init -)"

# Perl
eval "$(plenv init -)"

# 手元のツール類
export PATH="$HOME/local/bin:$PATH"

# 環境変数の設定
export LIBRARY_PATH="$(brew --prefix libgccjit)/lib/gcc/11"

export LDFLAGS="-L/usr/local/opt/readline/lib"
export CPPFLAGS="-I/usr/local/opt/readline/include"
export PKG_CONFIG_PATH="/usr/local/opt/readline/lib/pkgconfig"

export EDITOR="emacsclient -nw"
