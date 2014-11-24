if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

PATH="$PATH":/opt/homebrew-cask/Caskroom/android-studio-bundle/0.8.0 build-135.1245622/Android Studio.app/sdk

JAVA_HOME=`/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home -v "1.8"`

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export PGDATA=/usr/local/var/postgres

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/Mikio/.gvm/bin/gvm-init.sh" ]] && source "/Users/Mikio/.gvm/bin/gvm-init.sh"

