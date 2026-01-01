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

# mise
~/.local/bin/mise activate fish | source

# aqua
set -gx AQUA_ROOT_DIR (aqua root-dir)
set -gx AQUA_GLOBAL_CONFIG $HOME/.config/aquaproj-aqua/aqua.yaml
fish_add_path (aqua root-dir)/bin

# pnpm
set -gx PNPM_HOME $HOME"/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
    fish_add_path $PNPM_HOME
end
# pnpm end
