#!/bin/bash 
# Install script for packages in my bare arch linux setup 

# Update the system
sudo pacman -Syu archlinux-keyring

# 1. Install essential packages
sudo pacman -S base-devel intel-ucode git vim zsh zsh-completions zsh-autosuggestions zsh-syntax-highlighting bash-completion openssh wget curl htop neofetch bat exa fd ripgrep fzf stow stylelua tar tree time acpilight aria2 unrar unzip bluez bluez-utils blueman brightnessctl xfsprogs ntfs-3g clang gcc clipmenu clipnotify dunst e2fsprogs gvfs efibootmgr zoxide gc lfs git-lfs gnome-keyring polkit-kde-agent-polkit pass udiskie gstreamer jq xdotool screenkey xorg-xprop lazygit lolcat sxiv shellcheck net-tools numlockx preload prettier progress rsync sddm trash-cli tlp tlp-rdw neovim xorg-xinput xclip xcompmgr xorg-xrandr xorg-xsetroot xsel xwallpaper pandoc starship --noconfirm

# 2. Install paru 
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si 
cd ..
rm -rf paru

# 3. Install AUR packages
paru -S betterlockscreen-git brave-bin floorp-bin ccrypt didyoumean-git github-desktop-bin networkmanager-dmenu-git visual-studio-code-bin --noconfirm

# 4. Install GUI packages
sudo pacman -S baobab gnome-disk-utility flameshot bc discord docker emacs gparted libreoffice-fresh lxappearance pavucontrol qutebrowser ranger yad telegram-desktop timeshift --noconfirm

# 5. Install multimedia packages
sudo pacman -S mpv cava mpc mpd ncmpcpp mplayer poppler poppler-glib --noconfirm

# 6. Install fonts 
sudo pacman -S adobe-source-code-pro-fonts noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-hack ttf-jetbrains-mono ttf-ubuntu-font-family ttf-ubuntu-mono-nerd ttf-ubuntu-nerd ttf-opensans gnu-free-fonts --noconfirm

# 7. Install external packages
paru -S ani-cli-git arch-wiki-docs ytfzf-git firefox-pwa-bin walogram-git --noconfirm
sudo pacman -S yt-dlp hugo hunspell hunspell-en_us imagemagick ueberzug luacheck mlocate newsboat nodejs npm texlive-bin texlive-meta texlive-latex texlive-basic translate-shell --noconfirm

# 8. Permissions
sudo usermod -aG docker $USER
sudo usermod -aG video $USER 

# 9. Enable services
sudo updatedb
sudo systemctl enable --now tlp

# 10 Mariadb setup
# sudo pacman -S mariadb 
# sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
# sudo systemctl enable mariadb 
# sudo systemctl start mariadb 
# sudo mysql_secure_installation 

# 11. Setup git 
git config --global user.name "Chaganti-Reddy"
git config --global user.email "chagantivenkataramireddy4@gmail.com"

# Tips:
# 1. Install Java 
# sudo pacman -S jdk-openjdk openjdk-doc openjdk-src --noconfirm
# 2. Install qbit torrent
# sudo pacman -S qbittorrent --noconfirm
# 3. Install qt5ct
# sudo pacman -S qt5ct --noconfirm
# 4. Install Teamviewer
# paru -S teamviewer --noconfirm
# 5. Install Zathura
# sudo pacman -S zathura zathura-pdf-mupdf zathura-djvu zathura-ps zathura-cb --noconfirm && paru -S zathura-pywal-git
# 6. Install Okular 
# sudo pacman -S okular --noconfirm
# 7. Install Dolphin 
# sudo pacman -S dolphin --noconfirm
# 8. Install GTK theme and QT theme
# sudo pacman -S arc-gtk-theme arc-icon-theme qt5-styleplugins --noconfirm
# 9. Install anipy-cli 
# paru -S anipy-cli-git --noconfirm
# 10. Install Doom Emacs 
# git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install
# 11. Insatll waldl from Extras folder of dotfiles
# cd ./Extras/Extras/waldl-master/ && sudo make install && cd ../../..
# 12. Install ollama from Extras folder of dotfiles
# sh ./Extras/Extras/ollama.sh 
