# aliases
if test (uname) = "Linux"
    alias open "xdg-open"
end

alias f "open ."
alias emacs "emacsclient -nw"

# set PATH so it includes user's private bin if it exists
if test -d $HOME/bin
    fish_add_path $HOME/bin
end

# set PATH so it includes user's private bin if it exists
if test -d $HOME/.local/bin
    fish_add_path $HOME/.local/bin
end

# pnpm
set -gx PNPM_HOME "/home/benevolent0505/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

~/.local/bin/mise activate fish | source
