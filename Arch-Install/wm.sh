sudo timedatectl set-ntp true
sudo hwclock --systohc

sudo pacman -Syy

echo "MAIN PACKAGES"

sudo pacman -S emacs-nativecomp clang lolcat exa fish flameshot ueberzug tcl tk ranger fzf wget bspwm sxhkd pandoc nitrogen lxappearance arandr pulseaudio-alsa pavucontrol dina-font tamsyn-font ttf-bitstream-vera ttf-croscore ttf-dejavu ttf-droid gnu-free-fonts ttf-ibm-plex ttf-liberation ttf-linux-libertine noto-fonts ttf-roboto tex-gyre-fonts ttf-ubuntu-font-family ttf-anonymous-pro ttf-cascadia-code ttf-fantasque-sans-mono ttf-fira-mono ttf-hack ttf-fira-code ttf-inconsolata ttf-jetbrains-mono ttf-monofur inter-font ttf-opensans gentium-plus-font ttf-junicode adobe-source-han-sans-otc-fonts adobe-source-han-serif-otc-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome awesome-terminal-fonts rofi mpc mpd ncmpcpp polkit-gnome dunst pacman-contrib bluez bluez-utils qemu virt-manager python python-pip python-wheel ebtables starship iwd htop lua xclip neofetch mariadb ripgrep fd swtpm bat hwinfo rofi-emoji rofimoji rofi-calc expac baobab gnome-disk-utility python-pywal sxiv feh amfora sddm texlive-most texlive-lang yad discord vlc libreoffice-fresh python-pylint numlockx arch-wiki-docs avidemux-qt grub-customizer plantuml libvterm acpilight qt5ct kvantum-qt5 pass rofi-pass php pwgen python-black python-pyflakes python-isort python-pipenv python-pytest shellcheck  trayer nim ncdu mlocate mplayer ispell jdk-openjdk jre-openjdk hsetroot r  nodejs-lts-gallium npm cmatrix xscreensaver gpick lfs git-lfs translate-shell slop xwallpaper imv youtube-dl mpv python2 pcmanfm-qt file-roller python-opengl virtualbox  virtualbox-guest-iso virtualbox-host-dkms qtile xorg-server xorg-xinit xterm awesome conky spice-vdagent
# sudo pacman -S xfce4-terminal zsh xfce4-power-manager feh cronie lightdm kitty jq volumeicon languagetool ifuse i3-gaps r

# sudo systemctl enable lightdm
sudo systemctl enable sddm
sudo systemctl enable iwd.service
sudo systemctl enable libvirtd
sudo systemctl start libvirtd

sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
sudo systemctl enable --now mariadb
sudo mysql_secure_installation
# after installing that add IgnorePkg   = mariadb* to pacman.conf to stop autoupdating in pacman -Syu
# So that your database is not at risk and you can update it manually 
# after upgrading manually use systemctl restart mariadb and mysql_upgrade -u root -p
# that's it.
# Now to launch the sql database use sudo mysql -u username -p

sudo corepack enable
sudo updatedb

paru -S pass-otp ksuperkey zathura zathura-djvu zathura-pdf-mupdf zathura-ps zathura-cb polybar-git qalculate-gtk ttf-icomoon-feather shell-color-scripts zoxide alacritty-ligatures-git ttf-mononoki visual-studio-code-bin imagemagick qbittorrent brave-bin fisher fnm-bin telegram-desktop-bin nerd-fonts-mononoki zig android-tools android-udev lazygit libxft-bgra noto-fonts-emoji pamixer prettier screenkey stow tree ttf-joypixels ttf-unifont wireless_tools xbanish stylua xsel picom-jonaburg-git udiskie github-desktop-bin otf-raleway otf-font-awesome otf-font-awesome timeshift-bin xmonad xmobar xmonad-contrib graphviz tidy stylelint auctex-latexmk gromit-mpx gopls gomodifytags gotest gore jq appimagelauncher-git mu mbsync-git sddm-sugar-candy-git betterlockscreen-git ttf-overpass ttf-juliamono nvm didyoumean reddio texlive-tlpdb ttf-ms-fonts virtualbox-ext-oracle whitesur-icon-theme whitesur-gtk-theme-git whitesur-cursor-theme-git lain-git
#yay -S  protonvpn filezilla pamac-all obsidian sublime-text-4 gromit-mpx lightdm-webkit2-greeter newsflash newsflash newsflash raven-reader-bin beekeeper-studio-bin mailspring stockfish

#sudo systemctl enable snapd.service
sudo systemctl enable bluetooth.service
sudo virsh net-start default
sudo virsh net-autostart default
sudo usermod -aG video reddy
sudo usermod -aG vboxusers reddy
#sudo systemctl enable cronie.service --now

# If you want install doo emacs by uncommenting it
# git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
# ~/.emacs.d/bin/doom install

# If you want to install python packages uncomment this
pip install pandas seaborn keras numpy jovian wordcloud jedi nltk tensorflow imutils matplotlib scikit-learn sklearn jupyterlab notebook voila JLDracula opencv-python numba urllib3 pygame nose python-lsp-server[all] debugpy radian plotly statsmodels "jupyterlab>=3" "ipywidgets>=7.6" jupyter-dash kaleido yellowbrick tqdm cufflinks wakatime torch torchvision torchaudio black opendatasets folium ptvsd iwlib
pip3 install jupyter-tabnine --user
jupyter nbextension install --py jupyter_tabnine --user
jupyter nbextension enable --py jupyter_tabnine --user
jupyter serverextension enable --py jupyter_tabnine --user
# R packages
# install.packages("languageserver")
# install.packages("httpgd")
# install.packages("jsonlite")
paru -S js-beautify
pip install "jupyterlab-kite>=2.0.2"
sudo modprobe vboxdrv

#source /usr/share/nvm/nvm.sh
#source /usr/share/nvm/bash_completion
#source /usr/share/nvm/install-nvm-exec
sudo pacman -R i3-wm
printf "\e[1;32mCHANGE NECESSARY FILES BEFORE REBOOT\e[0m"
