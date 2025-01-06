#!/bin/env bash
set -e

# Introduction & Warning
echo "Welcome to my Arch Setup!" && sleep 2
echo "Some parts of the script require sudo, so if you're planning on leaving the desktop while the installation script does its thing, better drop it already!." && sleep 4

# Creating all required home directories if not present
mkdir ~/Downloads ~/Documents ~/Music ~/Pictures ~/Videos ~/Templates

echo "Setting up pacman.conf..."

# Use sed to find the #UseSyslog line and append the settings under it
sudo sed -i '/#UseSyslog/a\
ILoveCandy\nParallelDownloads=10\nColor' /etc/pacman.conf

echo "Configuration added under #UseSyslog."

# System update
echo "Performing a full system update..."
sudo pacman --noconfirm -Syu
clear
echo "System update done" && sleep 2
clear

# Install Git if not present
echo "Installing git..." && sleep 1
sudo pacman -S --noconfirm --needed git
clear

# Clone and install Paru if not installed
echo "This script requires an AUR helper to install the dependencies. Installing paru..." &
sleep 2
if ! command -v paru &>/dev/null; then
  echo "Installing Paru, an AUR helper..."
  cd Downloads
  git clone https://aur.archlinux.org/paru.git
  cd paru
  makepkg -si
  cd ..
  echo "Paru installed"
  rm -rf paru
  cd ~/dotfiles
fi
clear

echo "Checking and removing packages: dolphin, htop, and wofi..."

# Array of packages to check and remove
packages=("dolphin" "htop" "wofi")

for pkg in "${packages[@]}"; do
  if pacman -Qi "$pkg" &>/dev/null; then
    echo "$pkg is installed. Removing..."
    sudo pacman -Rns --noconfirm "$pkg"
    dialog --msgbox "$pkg has been removed successfully." 10 50
  else
    echo "$pkg is not installed."
  fi
done

dialog --msgbox "Package check and removal process is complete." 10 50

clear

# Install base-devel and required packages
echo "Installing dependencies.." && sleep 2

sudo pacman -S --noconfirm --needed base-devel intel-ucode vim zsh zsh-completions zsh-autosuggestions zsh-syntax-highlighting bash-completion openssh wget curl btop fastfetch bat exa fd ripgrep fzf stow stylua tar tree time acpilight aria2 unrar unzip bluez bluez-utils brightnessctl xfsprogs ntfs-3g clang gcc clipmenu clipnotify inotify-tools psutils dunst e2fsprogs gvfs gvfs-afc gvfs-google gvfs-goa gvfs-gphoto2 gvfs-mtp gvfs-nfs gvfs-onedrive gvfs-smb efibootmgr zoxide gc git-lfs gnome-keyring polkit-gnome pass udiskie gstreamer jq xdotool screenkey xorg-xprop lazygit lolcat sxiv shellcheck net-tools numlockx prettier progress zip rsync trash-cli tlp tlp-rdw neovim feh xorg-xinput xclip xcompmgr xorg-xrandr xorg-xsetroot xsel xwallpaper pandoc starship python-pywal glow xarchiver xfce4-clipman-plugin libguestfs bc xorg-xman man-db man-pages ncdu python-adblock dnsmasq python-pip nwg-look python-prctl vscode-css-languageserver ffmpegthumbnailer lua-language-server pass pinentry gnupg pass-otp zbar xorg-xlsclients xscreensaver os-prober qt5ct pamixer qt5-wayland qt6-wayland parallel shfmt tesseract html-xml-utils cava thunar thunar-archive-plugin thunar-media-tags-plugin thunar-volman thunar-vcs-plugin flameshot alacritty playerctl ncmpcpp mpd mpv mpc poppler poppler-glib adobe-source-code-pro-fonts noto-fonts noto-fonts-cjk noto-fonts-emoji ttf-hack ttf-jetbrains-mono ttf-ubuntu-font-family ttf-ubuntu-mono-nerd ttf-ubuntu-nerd ttf-opensans gnu-free-fonts libnewt baobab gnome-disk-utility gparted pavucontrol ranger yad timeshift go hugo hunspell hunspell-en_us imagemagick ueberzug luacheck yt-dlp mlocate nodejs npm translate-shell jdk-openjdk openjdk-doc openjdk-src blueman zenity rofi rofi-emoji rofi-calc newsboat

paru -S --noconfirm --needed base-devel python-psutil preload git-remote-gcrypt pywal-git picom ttf-ms-fonts qt6ct-kde ccrypt didyoumean-git arch-wiki-docs material-black-colors-theme apple_cursor kvantum-theme-materia kvantum --noconfirm

echo "Dependencies installed... executing services & permissions..." && sleep 1

sudo updatedb
sudo mandb
sudo systemctl enable --now tlp
sudo systemctl enable --now bluetooth.service

sudo usermod -aG video "$USER"

echo "For VM sharing details https://docs.getutm.app/guest-support/linux/"


# paru -S material-black-colors-theme apple_cursor kvantum-theme-materia kvantum --noconfirm

clear

# --------------------------------------------------------------------------------------

echo "Setting up Git configuration..."

# Ask user whether to proceed with Git setup
if ! whiptail --title "Git Configuration Setup" --yesno "Do you want to proceed with setting up Git configuration?" 10 60; then
  echo "Git setup canceled. Exiting."
  exit 1
fi

# Prompt for Git username using whiptail
git_username=$(whiptail --inputbox "Enter your Git username:" 10 60 3>&1 1>&2 2>&3)
exit_status=$?
if [ $exit_status -ne 0 ] || [ -z "$git_username" ]; then
  whiptail --msgbox "Git username setup canceled or empty. Exiting." 10 50
  exit 1
fi

# Prompt for Git email using whiptail
git_email=$(whiptail --inputbox "Enter your Git email:" 10 60 3>&1 1>&2 2>&3)
exit_status=$?
if [ $exit_status -ne 0 ] || [ -z "$git_email" ]; then
  whiptail --msgbox "Git email setup canceled or empty. Exiting." 10 50
  exit 1
fi

# Display confirmation dialog
if whiptail --yesno "Please confirm the Git configuration:\n\nUsername: $git_username\nEmail: $git_email" 12 60; then
  # Set Git configuration if confirmed
  git config --global user.name "$git_username"
  git config --global user.email "$git_email"
  whiptail --msgbox "Git has been successfully configured:\n\nUsername: $git_username\nEmail: $git_email" 12 60
else
  whiptail --msgbox "Git configuration canceled. No changes were made." 10 50
  exit 1
fi

# -------------------------------------------------------------------------------------

echo "Installing main packages and applications that I personally use the most..." && sleep 1

# Function to display the browser selection menu using dialog
install_browser() {
  # Display the checklist menu
  choices=$(dialog --title "Browser Selection" \
    --checklist "Select the browsers you want to install:" 15 50 6 \
    "Zen-Browser" "" OFF \
    "Firefox" "" OFF \
    "Chromium" "" OFF \
    "Vivaldi" "" OFF \
    "qutebrowser" "" OFF \
    "Brave" "" OFF \
    3>&1 1>&2 2>&3)

  # Check if the user pressed Cancel
  if [ $? -ne 0 ]; then
    echo "No selection made. Exiting."
    exit 1
  fi

  # Process the selected options
  for choice in $choices; do
    case $choice in
    "Zen-Browser")
      echo "Installing Zen-Browser..."
      paru -S --noconfirm --needed zen-browser-bin
      # Video Download Helper
      curl -sSLf https://github.com/aclap-dev/vdhcoapp/releases/latest/download/install.sh | bash
      ;;
    "Firefox")
      echo "Installing Firefox..."
      sudo pacman -S --noconfirm firefox
      ;;
    "Chromium")
      echo "Installing Chromium..."
      sudo pacman -S --noconfirm chromium
      ;;
    "Vivaldi")
      echo "Installing Vivaldi..."
      paru -S --noconfirm vivaldi
      ;;
    "qutebrowser")
      echo "Installing qutebrowser..."
      sudo pacman -S --noconfirm qutebrowser
      ;;
    "Brave")
      echo "Installing Brave..."
      paru -S --noconfirm brave-bin
      ;;
    *)
      echo "Invalid choice: $choice"
      ;;
    esac
  done
}

# Call the browser installation function
install_browser

dialog --msgbox "Selected browsers have been installed." 10 50

clear

# -------------------------------------------------------------------------------------

echo "Setting up Miniconda..."

# Ask the user if they want to install Miniconda
dialog --title "Install Miniconda?" \
  --yesno "Would you like to install Miniconda? (Recommended for Python and Data Science)" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "Miniconda installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "Miniconda installation will begin now." 10 50

  # Download Miniconda installer
  wget https://repo.anaconda.com/miniconda/Miniconda3-py310_24.3.0-0-Linux-x86_64.sh

  # Run the installer
  bash Miniconda3-py310_24.3.0-0-Linux-x86_64.sh

  # Remove the installer after installation
  rm Miniconda3-py310_24.3.0-0-Linux-x86_64.sh

  dialog --msgbox "Miniconda installation completed." 10 50
fi

clear

# -------------------------------------------------------------------------------------

echo "Setting up KVM..."

# Ask the user if they want to install Miniconda
dialog --title "Install KVM QEMU?" \
  --yesno "Would you like to install KVM QEMU Virtual Machine" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "KVM installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "KVM installation will begin now." 10 50

sudo pacman -S qemu-full qemu-img libvirt virt-install virt-manager virt-viewer spice-vdagent edk2-ovmf dnsmasq swtpm guestfs-tools libosinfo tuned
  sudo systemctl enable --now libvirtd.service
  sudo usermod -aG libvirt "$USER"
sudo virsh net-autostart default

sudo modprobe -r kvm_intel
sudo modprobe kvm_intel nested=1

  dialog --msgbox "QEMU installation completed." 10 50
fi

clear

# -------------------------------------------------------------------------------------

echo "Setting up torrent and remote working applications..."

# Display checklist for torrent and remote working applications
apps=$(whiptail --title "Select Applications to Install" \
  --checklist "Choose the applications you want to install:" 20 60 8 \
  "torrent-cli" "" OFF \
  "qBittorrent" "" OFF \
  "Transmission" "" OFF \
  "Remmina" "" OFF \
  "VNC" "" OFF \
  "TeamViewer" "" OFF \
  "AnyDesk" "" OFF \
  "xrdp" "" OFF \
  "openvpn" "" OFF \
  3>&1 1>&2 2>&3)

exit_status=$?
if [ $exit_status -ne 0 ] || [ -z "$apps" ]; then
  whiptail --msgbox "No applications selected or setup canceled. Exiting." 10 50
  exit 1
fi

# Process selected applications and install them
for app in $apps; do
  case $app in
  "torrent-cli")
    echo "Installing torrent-cli..."
    paru -S --noconfirm --needed webtorrent-cli webtorrent-mpv-hook peerflix
    ;;
  "qBittorrent")
    echo "Installing qBittorrent..."
    sudo pacman -S --noconfirm qbittorrent
    ;;
  "Transmission")
    echo "Installing Transmission..."
    sudo pacman -S --noconfirm transmission-qt
    ;;
  "Remmina")
    echo "Installing Remmina (Remote Desktop Client)..."
    sudo pacman -S --noconfirm remmina
    ;;
  "VNC")
    echo "Installing VNC Server..."
    sudo pacman -S --noconfirm tigervnc
    ;;
  "TeamViewer")
    echo "Installing TeamViewer..."
    sudo pacman -S --noconfirm teamviewer
    ;;
  "AnyDesk")
    echo "Installing AnyDesk..."
    sudo pacman -S --noconfirm anydesk
    ;;
  "xrdp")
    echo "Installing xrdp (Remote Desktop Protocol)..."
    sudo pacman -S --noconfirm xrdp
    ;;
  "openvpn")
    echo "Installing OpenVPN..."
    sudo pacman -S --noconfirm openvpn
    ;;
  *)
    echo "Invalid choice: $app"
    ;;
  esac
done

whiptail --msgbox "Selected torrents have been installed." 10 50

clear

# -------------------------------------------------------------------------------------

echo "Setting up development tools, office tools, communication tools, and multimedia tools..."

# Display checklist with a single column (scrollable) for all tools using dialog
tools_choices=$(dialog --title "Select Tools to Install" \
  --checklist "Choose the tools you want to install:" 20 60 18 \
  "Visual-Studio-Code" "" OFF \
  "GitHub-Desktop" "" OFF \
  "Docker" "" OFF \
  "Docker-Desktop" "" OFF \
  "Kubernetes" "" OFF \
  "Latex" "" OFF \
  "Discord" "" OFF \
  "Telegram" "" OFF \
  "LibreOffice" "" OFF \
  "OnlyOffice" "" OFF \
  "Skype" "" OFF \
  "Slack" "" OFF \
  "Zoom" "" OFF \
  "Blender" "" OFF \
  "Octave" "" OFF \
  "OBS-Studio" "" OFF \
  "Inkscape" "" OFF \
  "GIMP" "" OFF \
  "VLC" "" OFF \
  "Audacity" "" OFF \
  "Krita" "" OFF \
  "Shotcut" "" OFF \
  "Steam" "" OFF \
  "Minecraft" "" OFF \
  "YouTUI" "TUI YT Music(Preferred)" OFF \
  "YTerMusic" "TUI YT Music" OFF \
  3>&1 1>&2 2>&3)

exit_status=$?
if [ $exit_status -ne 0 ] || [ -z "$tools_choices" ]; then
  dialog --msgbox "No tools selected or setup canceled. Exiting." 10 50
  exit 1
fi

# Process selected tools and install them
for app in $tools_choices; do
  case $app in
  "Visual-Studio-Code")
    echo "Installing Visual Studio Code..."
    paru -S --noconfirm --needed visual-studio-code-bin
    ;;
  "GitHub-Desktop")
    echo "Installing GitHub Desktop..."
    paru -S --noconfirm --needed github-desktop-bin
    ;;
  "Docker")
    echo "Installing Docker..."
    sudo pacman -S --noconfirm docker docker-compose
    sudo systemctl enable --now docker.service
    sudo usermod -aG docker "$USER"
    ;;
  "Docker-Desktop")
    echo "Installing Docker Desktop..."
    paru -S --noconfirm docker-desktop
    ;;
  "Kubernetes")
    echo "Installing Kubernetes..."
    paru -S kind-bin
    curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
    ;;
  "Latex")
    echo "Installing LaTeX (texlive)..."
    sudo pacman -S texlive-bin texlive-meta texlive-latex texlive-basic texlive-binextra perl-yaml-tiny perl-file-homedir perl-unicode-linebreak
    ;;
  "Discord")
    echo "Installing Discord..."
    sudo pacman -S --noconfirm discord
    ;;
  "Telegram")
    echo "Installing Telegram..."
    paru -S --noconfirm telegram-desktop-bin
    ;;
  "LibreOffice")
    echo "Installing LibreOffice..."
    sudo pacman -S --noconfirm libreoffice-fresh
    ;;
  "OnlyOffice")
    echo "Installing OnlyOffice..."
    paru -S --noconfirm onlyoffice-bin
    ;;
  "Skype")
    echo "Installing Skype..."
    paru -S --noconfirm skypeforlinux-bin
    ;;
  "Slack")
    echo "Installing Slack..."
    paru -S --noconfirm slack-desktop
    ;;
  "Zoom")
    echo "Installing Zoom..."
    paru -S --noconfirm zoom
    ;;
  "Blender")
    echo "Installing Blender..."
    sudo pacman -S --noconfirm blender
    ;;
  "Octave")
    echo "Installing Octave..."
    sudo pacman -S --noconfirm octave
    ;;
  "OBS-Studio")
    echo "Installing OBS Studio..."
    sudo pacman -S --noconfirm obs-studio
    ;;
  "Inkscape")
    echo "Installing Inkscape..."
    sudo pacman -S --noconfirm inkscape
    ;;
  "GIMP")
    echo "Installing GIMP..."
    sudo pacman -S --noconfirm gimp
    ;;
  "VLC")
    echo "Installing VLC..."
    sudo pacman -S --noconfirm vlc
    ;;
  "Audacity")
    echo "Installing Audacity..."
    sudo pacman -S --noconfirm audacity
    ;;
  "Krita")
    echo "Installing Krita..."
    sudo pacman -S --noconfirm krita
    ;;
  "Shotcut")
    echo "Installing Shotcut..."
    sudo pacman -S --noconfirm shotcut
    ;;
  "Steam")
    echo "Installing Steam..."
    paru -S --noconfirm steam
    ;;
  "Minecraft")
    echo "Installing Minecraft..."
    paru -S --noconfirm minecraft-launcher
    ;;
  "YouTUI")
    echo "Installing YouTUI..."
    cargo install youtui
    ;;
  "YTerMusic")
    echo "Installing YTerMusic..."
    cargo install ytermusic
    ;;
  *)
    echo "Invalid choice: $app"
    ;;
  esac
done

dialog --msgbox "Selected tools have been installed." 10 50

clear

# -------------------------------------------------------------------------------------

echo "Setting up additional tools and packages..."

# Display checklist with a single column (scrollable) for additional tools using dialog
extra_tools_choices=$(dialog --title "Select Extra Tools to Install" \
  --checklist "Choose the extra tools and packages you want to install:" 20 60 20 \
  "Ani-Cli" "" OFF \
  "Ani-Cli-PY" "Python Version" OFF \
  "ytfzf" "" OFF \
  "Zathura" "" OFF \
  "Evince" "" OFF \
  "Okular" "" OFF \
  "Foxit-Reader" "" OFF \
  "Master-PDF-Editor" "" OFF \
  "MuPDF" "" OFF \
  3>&1 1>&2 2>&3)

exit_status=$?
if [ $exit_status -ne 0 ] || [ -z "$extra_tools_choices" ]; then
  dialog --msgbox "No tools selected or setup canceled. Exiting." 10 50
  exit 1
fi

# Process selected tools and install them
for app in $extra_tools_choices; do
  case $app in
  "Ani-Cli")
    echo "Installing ani-cli..."
    paru -S --noconfirm --needed ani-cli-git
    ;;
  "Ani-Cli-PY")
    echo "Installing ani-cli python version..."
    pip install anipy-cli
    ;;
  "ytfzf")
    echo "Installing ytfzf..."
    paru -S --noconfirm --needed ytfzf-git
    ;;
  "Zathura")
    echo "Installing Zathura..."
    sudo pacman -S zathura zathura-pdf-mupdf zathura-djvu zathura-ps zathura-cb --noconfirm
    ;;
  "Evince")
    echo "Installing Evince..."
    sudo pacman -S --noconfirm evince
    ;;
  "Okular")
    echo "Installing Okular..."
    sudo pacman -S --noconfirm okular
    ;;
  "Foxit-Reader")
    echo "Installing Foxit Reader..."
    paru -S --noconfirm foxitreader
    ;;
  "Master-PDF-Editor")
    echo "Installing Master PDF Editor..."
    paru -S --noconfirm masterpdfeditor
    ;;
  "MuPDF")
    echo "Installing MuPDF..."
    sudo pacman -S --noconfirm mupdf
    ;;
  *)
    echo "Invalid choice: $app"
    ;;
  esac
done

dialog --msgbox "Selected extra tools and packages have been installed." 10 50
clear

# -------------------------------------------------------------------------------------

echo "Setting up MariaDB..."

# Ask the user if they want to install MariaDB
dialog --title "Install MariaDB?" \
  --yesno "Would you like to install MariaDB (a relational database management system)?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "MariaDB installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "MariaDB installation will begin now." 10 50

  sudo pacman -S mariadb --noconfirm
  sudo mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql
  sudo systemctl enable --now mariadb
  sudo mariadb-secure-installation

  # Inform the user that MariaDB has been installed and configured
  dialog --msgbox "MariaDB has been installed and secured. You can now use it for your database needs." 10 50
fi

clear

# -------------------------------------------------------------------------------------

echo "Setting up Zsh..."

# Ask the user if they want to install Zsh
dialog --title "Install and Set Up Zsh?" \
  --yesno "Would you like to install Zsh and set it as your default shell?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "Zsh installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "Zsh installation will begin now." 10 50

  # Install Zsh if not already installed
  sudo pacman -S --noconfirm zsh zsh-completions zsh-autosuggestions zsh-syntax-highlighting

  # Change the default shell to Zsh
  chsh -s /bin/zsh

  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

  # Check if the installation was successful and prompt to restart the terminal
  dialog --msgbox "Zsh has been installed and set as your default shell. Please restart your terminal to apply the changes." 10 50
fi

clear

# --------------------------------------------------------------------------------------

echo "Setting up Hyprland..."

# Ask the user if they want to install Hyprland
dialog --title "Install Hyprland?" \
  --yesno "Would you like to install Hyprland (a dynamic Wayland compositor)?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "Hyprland installation skipped. Proceeding with the setup." 10 50
  exit 0
else
  dialog --msgbox "Hyprland installation will begin now." 10 50
fi

sudo pacman -S kitty system-config-printer hyprpicker hyprlock chafa hypridle waybar wl-clipboard speech-dispatcher hyprpaper brightnessctl cmake meson cpio grim slurp wtype wf-recorder swaync

paru -S wlrobs-hg hyprland-git xdg-desktop-portal-hyprland-git clipse-bin hyde-cli-git wlogout hyprshot-git hyprland-qtutils

sudo cp ~/dotfiles/Extras/Extras/nvim.desktop /usr/share/applications/

# Set up Hyprland configuration
echo "Configuring Hyprland..."

# Create the configuration file if it doesn't exist
if [ ! -f "$HOME/.config/hypr/hyprland.conf" ]; then
  cd ~/dotfiles
  stow Hyprland
  dialog --msgbox "Hyprland configuration has been set up." 10 50
else
  dialog --msgbox "Hyprland configuration file already exists." 10 50
fi

# Inform the user that installation is complete
dialog --msgbox "Hyprland has been installed and configured. Please restart your system to start using Hyprland." 10 50

clear

# -------------------------------------------------------------------------------------

echo "Setting up dwm..."

# Ask the user if they want to install dwm
dialog --title "Install dwm?" \
  --yesno "Would you like to install dwm (Dynamic Window Manager)?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "dwm installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "dwm installation will begin now." 10 50

  stow ~/dotfiles/suckless/

  cd ~/.config/dwm && sudo make clean install && cd
  cd ~/.config/slstatus && sudo make clean install && cd
  cd ~/.config/st && sudo make install && cd
  cd ~/.config/dmenu && sudo make install && cd
  cd ~/dotfiles/Extras/Extras/waldl-master/ && sudo make install && cd ~/dotfiles

  sudo mkdir /usr/share/xsessions/
  sudo cp ~/dotfiles/Extras/Extras/usr/share/xsessions/dwm.desktop /usr/share/xsessions

  # Inform the user that dwm has been installed
  dialog --msgbox "dwm has been installed. Please configure your system as needed." 10 50
fi

clear

# --------------------------------------------------------------------------------------

echo "Setting up Ollama..."

# Ask the user if they want to install Ollama
dialog --title "Install Ollama?" \
  --yesno "Would you like to install Ollama (a tool to run large language models locally)?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "Ollama installation skipped. Proceeding with the setup." 10 50
  exit 0
else
  dialog --msgbox "Ollama installation will begin now." 10 50
fi

# Check if Ollama is available through the AUR or if manual installation is needed
# You can install Ollama from their official site or via AUR
# We will use AUR (Arch User Repository) to install it using an AUR helper like paru.

# Install Ollama using paru (or an AUR helper of your choice)
if ! command -v ollama &>/dev/null; then
  echo "Ollama not found, installing Ollama from AUR..."

  # Install Ollama from the AUR using paru (if not already installed)
  paru -S --noconfirm ollama

  ollama serve
  ollama pull mistral
  ollama pull gemma:7b

  # Inform the user that Ollama has been installed
  dialog --msgbox "Ollama has been installed. You can now use it to run local large language models." 10 50
else
  dialog --msgbox "Ollama is already installed on your system." 10 50
fi

clear

# --------------------------------------------------------------------------------------

echo "Installing PIP Packages..."

# Ask the user if they want to install dwm
dialog --title "Install my PIP packages?" \
  --yesno "Would you like to install my PIP Packages?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "PIP packages installation skipped. Proceeding with the setup." 10 50
else
  dialog --msgbox "PIP packages installation will begin now." 10 50

  bash

  pip install pynvim numpy pandas matplotlib seaborn scikit-learn jupyterlab ipykernel ipywidgets tensorflow python-prctl inotify-simple psutil opencv-python keras mov-cli-youtube mov-cli mov-cli-test otaku-watcher film-central daemon jupyterlab_wakatime

  pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cpu # pytorch cpu version

  zsh

  # Inform the user that dwm has been installed
  dialog --msgbox "PIP Packages has been installed. Please configure your system as needed." 10 50
fi

clear

# --------------------------------------------------------------------------------------

echo "Setting up GRUB theme..."

# Ask if the user wants to set up a GRUB theme
dialog --title "Install GRUB theme?" \
  --yesno "Would you like to install a GRUB theme?" 10 60

exit_status=$?
if [ $exit_status -ne 0 ]; then
  dialog --msgbox "GRUB theme setup skipped. Proceeding with the setup." 10 50
  exit 0
else
  dialog --msgbox "GRUB theme setup will begin now." 10 50
fi

sudo cp ~/dotfiles/Extras/Extras/boot/grub/themes/SekiroShadow/ /boot/grub/themes/

# Enable the GRUB theme
echo "Setting GRUB theme..."
echo 'GRUB_THEME="/boot/grub/themes/SekiroShadow/theme.txt"' | sudo tee -a /etc/default/grub

echo "Enabling os-prober in GRUB configuration..."
echo 'GRUB_DISABLE_OS_PROBER="false"' | sudo tee -a /etc/default/grub

# Regenerate GRUB config
echo "Regenerating GRUB configuration..."
sudo grub-mkconfig -o /boot/grub/grub.cfg

dialog --msgbox "GRUB theme setup is complete and GRUB config has been updated." 10 50
clear

# --------------------------------------------------------------------------------------

echo "Setting up SDDM (Simple Desktop Display Manager)..."

# Install SDDM
echo "Installing SDDM..."

sudo pacman -S --noconfirm sddm qt6-5compat qt6-declarative qt6-svg

echo "Enabling SDDM to start at boot..."
sudo systemctl enable sddm.service --now

sudo cp ~/dotfiles/Extras/Extras/usr/share/sddm/themes/simple-sddm/ /usr/share/sddm/themes

sudo cp etc/sddm.conf /etc/sddm.conf

cd ~/dotfiles/

# Show confirmation message
dialog --msgbox "SDDM has been installed and enabled to start at boot." 10 50

clear

# --------------------------------------------------------------------------------------

echo "Setting Extra Packages for System..." && sleep 1

sudo npm i -g bash-language-server

cd ~/dotfiles/

rm ~/.bashrc ~/.zshrc

stow anipy-cli bashrc BTOP cava dunst face fastfetch DWMScripts flameshot gtk-3 Kvantum latexmkrc libreoffice mpd mpv ncmpcpp newsboat nvim NWG octave pandoc pavucontrol picom Profile qt6ct qutebrowser ranger redyt rofi screenlayout screensaver starship XFCEPic xsettingsd zathura zsh

clear

# --------------------------------------------------------------------------------------

echo "Setting up Fonts...
&& sleep 1

mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts

git clone https://github.com/Chaganti-Reddy/my-fonts.git

cd my-fonts

rm -rf .git/

cd ~/dotfiles/

echo "Fonts have been installed, moving some root files..."

cd ~/dotfiles/Extras/Extras/

sudo cp etc/nanorc /etc/nanorc
sudo cp etc/bash.bashrc /etc/bash.bashrc
sudo cp etc/DIR_COLORS /etc/DIR_COLORS
sudo cp etc/environment /etc/environment
sudo cp etc/mpd.conf /etc/mpd.conf

cp archcraft-dwm.zsh-theme ~/.oh-my-zsh/themes/archcraft-dwm.zsh-theme

# Setup kaggle JSON and wakatime files using ccrypt


# --------------------------------------------------------------------------------------

echo "All done! Please reboot your system."
