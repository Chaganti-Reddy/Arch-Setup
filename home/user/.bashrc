#
# ~/.bashrc
#
#neofetch
alias l="ls -la"
alias l+="xbacklight -inc"
alias l-="xbacklight -dec"
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
#
# # Arch latest news
# if [ "$PS1" ] && [[ $(ping -c1 www.google.com 2>&-) ]]; then
# 	# The characters "£, §" are used as metacharacters. They should not be encountered in a feed...
# 	echo -e "$(echo $(curl --silent https://www.archlinux.org/feeds/news/ | sed -e ':a;N;$!ba;s/\n/ /g') | \
# 		sed -e 's/&amp;/\&/g
# 		s/&lt;\|&#60;/</g
# 		s/&gt;\|&#62;/>/g
# 		s/<\/a>/£/g
# 		s/href\=\"/§/g
# 		s/<title>/\\n\\n\\n   :: \\e[01;31m/g; s/<\/title>/\\e[00m ::\\n/g
# 		s/<link>/ [ \\e[01;36m/g; s/<\/link>/\\e[00m ]/g
# 		s/<description>/\\n\\n\\e[00;37m/g; s/<\/description>/\\e[00m\\n\\n/g
# 		s/<p\( [^>]*\)\?>\|<br\s*\/\?>/\n/g
# 		s/<b\( [^>]*\)\?>\|<strong\( [^>]*\)\?>/\\e[01;30m/g; s/<\/b>\|<\/strong>/\\e[00;37m/g
# 		s/<i\( [^>]*\)\?>\|<em\( [^>]*\)\?>/\\e[41;37m/g; s/<\/i>\|<\/em>/\\e[00;37m/g
# 		s/<u\( [^>]*\)\?>/\\e[4;37m/g; s/<\/u>/\\e[00;37m/g
# 		s/<code\( [^>]*\)\?>/\\e[00m/g; s/<\/code>/\\e[00;37m/g
# 		s/<a[^§|t]*§\([^\"]*\)\"[^>]*>\([^£]*\)[^£]*£/\\e[01;31m\2\\e[00;37m \\e[01;34m[\\e[00;37m \\e[04m\1\\e[00;37m\\e[01;34m ]\\e[00;37m/g
# 		s/<li\( [^>]*\)\?>/\n \\e[01;34m*\\e[00;37m /g
# 		s/<!\[CDATA\[\|\]\]>//g
# 		s/\|>\s*<//g
# 		s/ *<[^>]\+> */ /g
# 		s/[<>£§]//g')\n\n";
# fi


# fnm
export PATH=/home/reddy/.local/bin:$PATH
export PATH=/home/reddy/.emacs.d/bin:$PATH
export PATH=/home/reddy/.local/go/bin:$PATH
export PATH=/home/reddy/.local/yarn/bin:$PATH
eval "`fnm env`"

export PATH=/home/reddy/SDK/flutter/bin:$PATH

source /usr/share/fzf/completion.bash
source /usr/share/fzf/key-bindings.bash
eval "$(starship init bash)"
#colorscript random
SUDO_EDITOR=/usr/bin/nvim
export SUDO_EDITOR


export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

function ctime(){
  g++ -std=c++17 $1".cpp" -o $1; 
  time ./$1
}

function mosscc(){
  perl -i moss.pl -l cc $1 $2
}

function mosspy(){
  perl -i moss.pl -l python $1 $2
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

cdnvm() {
    command cd "$@";
    nvm_path=$(nvm_find_up .nvmrc | tr -d '\n')

    # If there are no .nvmrc file, use the default nvm version
    if [[ ! $nvm_path = *[^[:space:]]* ]]; then

        declare default_version;
        default_version=$(nvm version default);

        # If there is no default version, set it to `node`
        # This will use the latest version on your machine
        if [[ $default_version == "N/A" ]]; then
            nvm alias default node;
            default_version=$(nvm version default);
        fi

        # If the current version is not the default version, set it to use the default version
        if [[ $(nvm current) != "$default_version" ]]; then
            nvm use default;
        fi

    elif [[ -s $nvm_path/.nvmrc && -r $nvm_path/.nvmrc ]]; then
        declare nvm_version
        nvm_version=$(<"$nvm_path"/.nvmrc)

        declare locally_resolved_nvm_version
        # `nvm ls` will check all locally-available versions
        # If there are multiple matching versions, take the latest one
        # Remove the `->` and `*` characters and spaces
        # `locally_resolved_nvm_version` will be `N/A` if no local versions are found
        locally_resolved_nvm_version=$(nvm ls --no-colors "$nvm_version" | tail -1 | tr -d '\->*' | tr -d '[:space:]')

        # If it is not already installed, install it
        # `nvm install` will implicitly use the newly-installed version
        if [[ "$locally_resolved_nvm_version" == "N/A" ]]; then
            nvm install "$nvm_version";
        elif [[ $(nvm current) != "$locally_resolved_nvm_version" ]]; then
            nvm use "$nvm_version";
        fi
    fi
}
alias cd='cdnvm'
cd "$PWD"
