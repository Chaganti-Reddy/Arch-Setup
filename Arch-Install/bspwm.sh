sudo timedatectl set-ntp true
sudo hwclock --systohc

sudo pacman -Syy

git clone https://aur.archlinux.org/paru.git
cd /paru
makepkg -si
rm -rf paru/

echo "MAIN PACKAGES"

sudo pacman -S neovim emacs clang lolcat exa gnome-chess fish bash-completion flameshot ueberzug tcl tk ranger fzf wget curl npm nodejs bspwm sxhkd pandoc nitrogen lxappearance arandr alsa-utils pulseaudio pulseaudio-alsa pavucontrol dina-font tamsyn-font ttf-bitstream-vera ttf-croscore ttf-dejavu ttf-droid gnu-free-fonts ttf-ibm-plex ttf-liberation ttf-linux-libertine noto-fonts ttf-roboto tex-gyre-fonts ttf-ubuntu-font-family ttf-anonymous-pro ttf-cascadia-code ttf-fantasque-sans-mono ttf-fira-mono ttf-hack ttf-fira-code ttf-inconsolata ttf-jetbrains-mono ttf-monofur adobe-source-code-pro-fonts cantarell-fonts inter-font ttf-opensans gentium-plus-font ttf-junicode adobe-source-han-sans-otc-fonts adobe-source-han-serif-otc-fonts noto-fonts-cjk noto-fonts-emoji ttf-font-awesome awesome-terminal-fonts rofi mpc mpd ncmpcpp polkit-gnome dunst pacman-contrib bluez bluez-utils qemu virt-manager python python-pip python-wheel ebtables starship iwd htop lua xclip neofetch mariadb ripgrep fd swtpm bat hwinfo rofi-emoji rofimoji rofi-calc expac baobab gnome-disk-utility python-pywal sxiv feh amfora sddm texlive-most texlive-lang yad discord vlc libreoffice-fresh

# sudo pacman -S xfce4-terminal zsh xfce4-power-manager feh cronie r lightdm kitty i3-gaps jq
sudo pacman -S acpilight cabal-install ccls haskell-language-server hoogle qt5ct kvantum-qt5 pass rofi-pass php pwgen python-black python-pyflakes python-isort python-pipenv python-pytest shellcheck slock trayer volumeicon nim ncdu mlocate inotify-tools dash

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

paru -S beekeeper-studio-bin mu mbsync-git pass-otp ksuperkey zathura zathura-djvu zathura-pdf-mupdf zathura-ps zathura-cb polybar-git qalculate-gtk ttf-icomoon-feather shell-color-scripts stockfish zoxide alacritty-ligatures-git ttf-mononoki visual-studio-code-bin imagemagick qbittorrent brave-bin fisher fnm-bin telegram-desktop-bin whatsdesk-bin nerd-fonts-mononoki zig android-tools android-udev lazygit libxft-bgra nautilus nautilus-admin-git noto-fonts-emoji pamixer prettier screenkey stow tree ttf-joypixels ttf-unifont wireless_tools xbanish stylua xsel picom-jonaburg-git udiskie github-desktop-bin sddm-sugar-candy-git otf-raleway otf-font-awesome newsflash otf-raleway otf-font-awesome pop-gtk-theme pop-icon-theme timeshift-bin xmonad xmobar xmonad-contrib nimsuggest-git  graphviz tidy  stylelint js-beautify auctex-latexmk gromit-mpx whitesur-cursor-theme-git

#yay -S  protonvpn filezilla pamac-all obsidian sublime-text-4 betterlockscreen-git gromit-mpx lightdm-webkit2-greeter  appimagelauncher-git newsflash 

#sudo systemctl enable snapd.service
sudo systemctl enable bluetooth.service
sudo virsh net-start default
sudo virsh net-autostart default
sudo usermod -aG video reddy
#sudo systemctl enable cronie.service --now

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

pip install keras numpy tensorflow imutils matplotlib scikit-learn sklearn jupyterlab notebook voila JLDracula opencv-python numba urllib3 pygame nose 

printf "\e[1;32mCHANGE NECESSARY FILES BEFORE REBOOT\e[0m"
