#!/bin/bash 
# Install script for packages in my bare arch linux setup 

# lines_to_append="ILoveCandy\nParallelDownloads=10\nColor"
# sudo sed -i '/^\[options\]/a '"$lines_to_append" /etc/pacman.conf

# Update the system
# sudo pacman -Syu archlinux-keyring --noconfirm

# 1. Install essential packages
# sudo pacman -S base-devel intel-ucode git vim zsh zsh-completions zsh-autosuggestions zsh-syntax-highlighting bash-completion openssh wget curl htop neofetch bat exa fd ripgrep fzf stow stylua tar tree time acpilight aria2 unrar unzip bluez bluez-utils blueman brightnessctl xfsprogs ntfs-3g clang gcc clipmenu clipnotify dunst e2fsprogs gvfs efibootmgr zoxide gc git-lfs gnome-keyring polkit-kde-agent pass udiskie gstreamer jq xdotool screenkey xorg-xprop lazygit lolcat sxiv shellcheck net-tools numlockx prettier progress rsync ly trash-cli tlp tlp-rdw neovim xorg-xinput xclip xcompmgr xorg-xrandr xorg-xsetroot xsel xwallpaper pandoc starship python-pywal glow xarchiver --noconfirm

# 2. Install paru 
# git clone https://aur.archlinux.org/paru.git
# cd paru
# makepkg -si
# cd ..
# rm -rf paru

# 3. Install AUR packages
# paru -S betterlockscreen-git brave-bin ccrypt didyoumean-git github-desktop-bin networkmanager-dmenu-git visual-studio-code-bin preload whitesur-cursor-theme-git --noconfirm

# 4. Install GUI packages
# sudo pacman -S baobab gnome-disk-utility flameshot bc discord docker emacs gparted libreoffice-fresh lxappearance pavucontrol qutebrowser ranger yad telegram-desktop timeshift --noconfirm

# 5. Install multimedia packages
# sudo pacman -S mpv mpc mpd ncmpcpp mplayer poppler poppler-glib --noconfirm && paru -S cava-git --noconfirm

# 6. Install fonts 
# sudo pacman -S adobe-source-code-pro-fonts noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-hack ttf-jetbrains-mono ttf-ubuntu-font-family ttf-ubuntu-mono-nerd ttf-ubuntu-nerd ttf-opensans gnu-free-fonts --noconfirm

# 7. Install external packages
# paru -S ani-cli-git arch-wiki-docs ytfzf-git firefox-pwa-bin walogram-git --noconfirm
# sudo pacman -S yt-dlp hugo hunspell hunspell-en_us imagemagick ueberzug luacheck mlocate newsboat nodejs npm texlive-bin texlive-meta texlive-latex texlive-basic translate-shell --noconfirm


# 8. Mariadb setup
# sudo pacman -S mariadb --noconfirm
# sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
# sudo systemctl enable mariadb
# sudo systemctl start mariadb
# sudo mariadb-secure-installation

# 9. Enable services
# sudo updatedb
# sudo systemctl enable --now tlp
# sudo systemctl enable --now bluetooth.service

# 10. Permissions
# sudo usermod -aG docker $USER
# sudo usermod -aG video $USER

# 11. Setup git 
# git config --global user.name "Chaganti-Reddy"
# git config --global user.email "chagantivenkataramireddy4@gmail.com"

# 12. Tips:
# 1. Install Java 
# sudo pacman -S jdk-openjdk openjdk-doc openjdk-src --noconfirm
# 2. Install qbit torrent
# sudo pacman -S qbittorrent --noconfirm
# 3. Install qt6ct
# sudo pacman -S qt6ct --noconfirm
# 4. Install Teamviewer
# paru -S teamviewer --noconfirm
# 5. Install Zathura
# sudo pacman -S zathura zathura-pdf-mupdf zathura-djvu zathura-ps zathura-cb --noconfirm && paru -S zathura-pywal-git
# 6. Install Okular 
# sudo pacman -S okular --noconfirm
# 7. Install Dolphin 
# sudo pacman -S dolphin --noconfirm
# 8. Install Thunar
# sudo pacman -S thunar thunar-archive-plugin thunar-volman thunar-media-tags-plugin
# 8. Install GTK theme and QT theme
# sudo pacman -S arc-gtk-theme arc-icon-theme --noconfirm
# 9. Install anipy-cli 
# paru -S anipy-cli-git --noconfirm
# 10. Install Doom Emacs 
# git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install
# 11. Insatll waldl from Extras folder of dotfiles
# cd  ~/dotfiles/Extras/Extras/waldl-master/ && sudo make install && cd ~/dotfiles
# 12. Install ollama from Extras folder of dotfiles
# sh ~/dotfiles/Extras/Extras/ollama.sh
# ollama serve
# ollama pull mistral
# ollama pull gemma:7b
# 13. Install brave Extensions
# brave://extensions/ ---> Install Comp Companion, uBlock Origin, GFG to Github, Google Translate, LeetHub, User-Agent switcher
# 14. Install Bash Language Server
# sudo npm i -g bash-language-server
# 15. Setup zsh shell as default
# chsh -s /bin/zsh
# 16. Install oh-my-zsh
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# 17. Install floorp instead of firefox 
# paru -S floorp-bin python-pywalfox


# 13. Move Respective files to root directory
# sudo cp ./Extras/Extras/usr/share/xsessions/dwm.desktop /usr/share/xsessions
# sudo cp -r ./Extras/Extras/boot/grub/themes/mocha /boot/grub/themes/
# Now edit the grub config file
# sudo cp ~/dotfiles/Extras/Extras/etc/bash.bashrc /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/DIR_COLORS /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/mpd.conf /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/nanorc /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/environment /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/ly/config.ini /etc/ly/
# cp ~/dotfiles/Extras/Extras/alanpeabody.zsh-theme ~/.oh-my-zsh/themes/

# 14. Setup PASS
# gpg --full-generate-key
# then setup pass init key

# 15. Install MINICONDA
# wget https://repo.anaconda.com/miniconda/Miniconda3-py310_24.3.0-0-Linux-x86_64.sh
# bash Miniconda3-py310_24.3.0-0-Linux-x86_64.sh
# rm Miniconda3-py310_24.3.0-0-Linux-x86_64.sh

# 16. Install Fonts
# mkdir ~/.local/share/fonts/
# git clone https://github.com/Chaganti-Reddy/my-fonts.git
# cp -r my-fonts ~/.local/share/fonts/
# rm -rf my-fonts

# 17. Install/stow dotfiles
# First check for conflicts
# rm -rf ~/.config/doom
# rm ~/.bashrc
# rm ~/.zshrc
# cd ~/dotfiles
# stow */
