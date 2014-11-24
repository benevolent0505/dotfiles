eval "$(rbenv init -)"

alias ls="ls -G"
alias ll="ls -lG"
alias ｌｓ="ls -G"
alias la="ls -A"
alias mr='rm'
alias be="bundle exec"
alias f="open ."
alias maketex2pdf="ptex2pdf -u -l"

eval "$(hub alias -s)"

# npm
PATH="$PATH":/usr/local/lib/node_modules/

PATH="$PATH":/opt/homebrew-cask/Caskroom/android-studio-bundle/"0.8.0 build-135.1245622"/"Android Studio.app"/sdk
ANDROID_HOME=/opt/homebrew-cask/Caskroom/android-studio-bundle/"0.8.0 build-135.1245622"/"Android Studio.app"/sdk
PATH="$PATH":$ANDROID_HOME/tools
PATH="$PATH":$ANDROID_HOME/platform-tools
PATH="$PATH":$ANDROID_HOME/build-tools/android-4.4W

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/Mikio/.gvm/bin/gvm-init.sh" ]] && source "/Users/Mikio/.gvm/bin/gvm-init.sh"
