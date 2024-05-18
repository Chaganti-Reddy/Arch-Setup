#!/bin/bash 
# Install script for dwm packages in my arch linux setup 
# Default using XFCE4 desktop in Arch Linux

# lines_to_append="ILoveCandy\nParallelDownloads=10\nColor"
# sudo sed -i '/^\[options\]/a '"$lines_to_append" /etc/pacman.conf

# # Change paru or yay here 
# pak="paru"

# Update the system
#sudo pacman -Syu archlinux-keyring --noconfirm

# 1. Install essential packages
#sudo pacman -S base-devel intel-ucode git vim zsh zsh-completions zsh-autosuggestions zsh-syntax-highlighting bash-completion openssh wget curl htop neofetch bat exa fd ripgrep fzf stow stylua tar tree time acpilight aria2 unrar unzip bluez bluez-utils brightnessctl xfsprogs ntfs-3g clang gcc clipmenu clipnotify inotify-tools psutils dunst e2fsprogs gvfs gvfs-afc gvfs-google gvfs-goa gvfs-gphoto2 gvfs-mtp gvfs-nfs gvfs-onedrive gvfs-smb efibootmgr zoxide gc git-lfs gnome-keyring polkit-gnome pass udiskie gstreamer jq xdotool screenkey xorg-xprop lazygit lolcat sxiv shellcheck net-tools numlockx prettier progress rsync lightdm-gtk-greeter trash-cli tlp tlp-rdw neovim xorg-xinput xclip xcompmgr xorg-xrandr xorg-xsetroot xsel xwallpaper pandoc starship python-pywal glow xarchiver xfce4-clipman-plugin qemu-full libguestfs xorg-xman man-db man-pages ncdu python-adblock dnsmasq firefox --noconfirm && sudo pacman -Rns parole --noconfirm

# Using XFCE4-CLIPMAN for clipboard manager

# 2. Install paru 
# git clone https://aur.archlinux.org/paru.git
# cd paru
# makepkg -si
# cd ..
# rm -rf paru

# 3. Install AUR packages
# pak -S betterlockscreen-git brave-bin ccrypt didyoumean-git github-desktop-bin visual-studio-code-bin preload peerflix webtorrent-cli webtorrent-mpv-hook lightdm-webkit-theme-litarvan lightdm-webkit2-greeter  --noconfirm

# 4. Install GUI packages
# sudo pacman -S baobab gnome-disk-utility flameshot bc discord docker gparted libreoffice-fresh pavucontrol qutebrowser ranger yad telegram-desktop timeshift --noconfirm

# 5. Install multimedia packages
# sudo pacman -S mpv mpc mpd ncmpcpp mplayer poppler poppler-glib --noconfirm && paru -S cava-git --noconfirm

# 6. Install fonts 
# sudo pacman -S adobe-source-code-pro-fonts noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-hack ttf-jetbrains-mono ttf-ubuntu-font-family ttf-ubuntu-mono-nerd ttf-ubuntu-nerd ttf-opensans gnu-free-fonts --noconfirm

# 7. Install external packages
# pak -S ani-cli-git arch-wiki-docs ytfzf-git walogram-git docker-desktop --noconfirm
# sudo pacman -S yt-dlp hugo hunspell hunspell-en_us imagemagick ueberzug luacheck mlocate newsboat nodejs npm texlive-bin texlive-meta texlive-latex texlive-basic translate-shell --noconfirm
# KDE components 
# sudo pacman -S kde-cli-tools kcmutils

# Install themes and icons 
# pak -S elementary-icon-theme --noconfirm # Previously used icons
# pak -S gruvbox-plus-icon-theme-git gruvbox-dark-gtk  && sudo pacman -S gtk-engine-murrine --noconfirm

# 8. Mariadb setup
# sudo pacman -S mariadb --noconfirm
# sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
# sudo systemctl enable mariadb
# sudo systemctl start mariadb
# sudo mariadb-secure-installation

# 9. Enable services
# sudo updatedb
# sudo mandb
# sudo systemctl enable --now tlp
# sudo systemctl enable --now bluetooth.service
# sudo systemctl enable lightdm.service
# sudo systemctl enable --now libvirtd

# 10. Permissions
# sudo usermod -aG docker $USER
# sudo usermod -aG video $USER
# sudo usermod -aG libvirt $USER
# sudo virsh net-start default

# 11. Setup git 
# git config --global user.name "Chaganti-Reddy"
# git config --global user.email "chagantivenkataramireddy4@gmail.com"

# 12. Alternatives & Optionals:
# 1. Install Java 
# sudo pacman -S jdk-openjdk openjdk-doc openjdk-src --noconfirm
# 2. Install qbit torrent
# sudo pacman -S qbittorrent --noconfirm
# 3. Install qt6ct
# sudo pacman -S qt6ct kvantum breeze-icons --noconfirm 
# sudo pacman -S lxappearance --noconfirm
# 4. Install Teamviewer
# paru -S teamviewer --noconfirm
# 5. Install Zathura
# sudo pacman -S zathura zathura-pdf-mupdf zathura-djvu zathura-ps zathura-cb --noconfirm && paru -S zathura-pywal-git
# 6. Install Okular 
# sudo pacman -S okular --noconfirm
# Or use FlatPak 
# sudo pacman -S flatpak 
# Restart the system 
# flatpak install flathub org.kde.okular
# 7. Install Dolphin 
# sudo pacman -S dolphin --noconfirm
# 8. Install Thunar
# sudo pacman -S thunar thunar-archive-plugin thunar-volman thunar-media-tags-plugin --noconfirm
# 8. Install GTK theme and QT theme
# sudo pacman -S arc-gtk-theme arc-icon-theme --noconfirm
# 9. Install anipy-cli 
# pak -S anipy-cli-git --noconfirm
# 10. Install Doom Emacs 
# sudo pacman -S emacs --noconfirm && git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs && ~/.config/emacs/bin/doom install # Later run doom sync
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
# exit # exit from zsh
# 17. Install floorp instead of firefox 
# pak -S floorp-bin python-pywalfox firefox-pwa-bin 
# 18. Use dmenu for network manager (Optional)
# pak -S networkmanager-dmenu-git 
# 19. Install Blender for Video Editing
# sudo pacman -S blender --noconfirm
# 20. Install OBS Studio for Screen Recording
# sudo pacman -S obs-studio --noconfirm
# 21. Install GIMP for Image Editing
# sudo pacman -S gimp --noconfirm
# 22. Install Inkscape for Vector Graphics
# sudo pacman -S inkscape --noconfirm
# 23. Install Octave for Numerical Computing
# sudo pacman -S octave --noconfirm  
# 24. Install blueman if needed for bluetooth manager 
# sudo pacman -S blueman --noconfirm 

# 13. Move Respective files to root directory
# sudo cp ~/dotfiles/Extras/Extras/usr/share/xsessions/dwm.desktop /usr/share/xsessions
# # sudo cp -r ~/dotfiles/Extras/Extras/boot/grub/themes/mocha /boot/grub/themes/
# sudo cp -r ~/dotfiles/Extras/Extras/boot/grub/themes/tartarus/ /boot/grub/themes/
# Now edit the grub config file
# sudo cp ~/dotfiles/Extras/Extras/etc/bash.bashrc /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/DIR_COLORS /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/mpd.conf /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/nanorc /etc/
# sudo cp ~/dotfiles/Extras/Extras/etc/environment /etc/
# sudo cp -r ~/dotfiles/Extras/Extras/etc/lightdm/ /etc/
# cp ~/dotfiles/Extras/Extras/alanpeabody.zsh-theme ~/.oh-my-zsh/themes/
# mkdir ~/.icons && cp -r ~/dotfiles/Extras/Extras/.icons/Capitaine Cursors (Gruvbox)/ ~/.icons/

# 14. Setup PASS
# gpg --full-generate-key && pass init $(gpg --list-keys --with-colons | grep '^pub:' | cut -d ':' -f 5)

# 15. Install MINICONDA
# zsh # start with zsh since we have to add into zsh
# wget https://repo.anaconda.com/miniconda/Miniconda3-py310_24.3.0-0-Linux-x86_64.sh
# sh Miniconda3-py310_24.3.0-0-Linux-x86_64.sh
# rm Miniconda3-py310_24.3.0-0-Linux-x86_64.sh
# exit

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

# 18. Install DWM
# cd ~/.config/dwm && sudo make clean install && cd
# cd ~/.config/slstatus && sudo make clean install && cd
# cd ~/.config/st && sudo make install && cd
# cd ~/.config/dmenu && sudo make install && cd

# 19. Install Lucas Chess 
# cd ~/Downloads/ && https://sourceforge.net/projects/lucaschessr/files/Version_R2/LucasChessR2_14j_LINUX.sh
# chmod +x LucasChessR2_14j_LINUX.sh 
# ./LucasChessR2_14j_LINUX.sh 
# 20. Install Stockfish 
# wget https://github.com/official-stockfish/Stockfish/releases/latest/download/stockfish-ubuntu-x86-64-avx2.tar
# tar -xvf stockfish-ubuntu-x86-64-avx2.tar
# mv stockfish ~/
# cd
# move lucaschess file to usr/bin 
# sudo cp ~/dotfiles/lucaschess /usr/bin/
# Or install pychess and add stockfish engine
# sudo pacman -S pychess
#
# 20. Use bottles for windows app 
# pak -S bottles-git

# Install python packages 
# pip install pynvim numpy pandas matplotlib seaborn scikit-learn jupyterlab ipykernel ipywidgets tensorflow python-prctl inotify-simple psutil opencv-python keras 
# pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu # pytorch cpu version 
