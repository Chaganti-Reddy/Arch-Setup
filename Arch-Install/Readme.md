# Network Configuration (WIFI)

1. Run iwctl.
2. Enter "station wlan0 get-network".
3. After finding network run "station wlan0 connect "network name"".
4. exit.

> Note: All the commands in the quotes should be type without quotes.

# Disk Configuration (UEFI , GPT Partitioning)

1. Enter these commands as below :

   > Here name = "/dev/diskname" (diskname example = sda or nvme1n1 etc. . ,)

1. gdisk name
1. Now enter the commands as per your disk space values with same arguments as shown in figure :
   ![This is Disk-Setup image](/assets/disk-setup.png)

1. After writing the changes to your disk type lsblk and that shows you the partitions
   ![This is lsblk image](/assets/after-disk.png)

1. Now next step is to allocate and mount the disks
   ![This is allocating image](/assets/allocating-disk.png)
   ![This is mounting image](/assets/mounting-disk.png)

# Installing Base Packages

1. Enter the command to install base packages:
   - pacstrap /mnt base linux-lts linux-lts-headers linux-firmware nano intel-ucode
     > If you want to install latest kernal replace linux-lts with linux, but i recommend to use linux-lts kernals to have more stability

> Also if you have an Amd machine you have to instal amd-ucode not to install intel-ucode 2. Now we have to generate the fstab so enter the command :

     * genfstab -U /mnt >> /mnt/etc/fstab
     * Now enter as root "arch-chroot /mnt"

# TimeZone Setup

1. Enter "timedatectl list timezone | grep --cityname--"
2. Myne is Asia/Kolkata timezone.
3. So type
   - ln -sf /usr/share/zoneinfo/Asia/Kolkata /etc/localtime
4. Now sync the clock "hwclock --systohc".

# UEFI Installation Script

1. Now clone this repository "https://github.com/Chaganti-Reddy/Arch-Setup.git"
2. Now enter into the folder and open base-uefi.sh file with editor.
   - cd Arch-Linux/Arch-Install
3. Now do changes for password for root and change the user name and password in that file only.
4. If you have a NVIDIA graphics card then uncomment the nvidia installation line in the script.
5. Now make the file executable
   - chmod +x base-uefi.sh
6. Now enter to root directory "cd /"
7. And run ./Arch-Linux/Arch-Install/base-uefi.sh
8. Now after completion of installation run the command "os-proper" to detect the dual-booting OS.
9. Now edit the mkinitcpio.conf file :
   - nano /etc/mkinitcpio.conf
10. Under modules section:
    - if you have nvidia :
      > Enter the nvidia in between the braces MODULES=(nvidia)
    - if you have intel :
      > Enter the i915 in between the braces MODULES=(i915)
    - if you have amd :
      > Enter the amdgpu in between the braces MODULES=(amdgpu)
    - if you have multiple like both intel and nvidia :
      > Enter the (nvidia i915) in between the braces MODULES=(nvidia i915)
11. Now we have to recreate the image:
    - Enter the command "mkinitcpio -p linux-lts"
      > If you installed linux the you have to enter "mkinitcpio -p linux"
12. Now change the grub file if you are using dual-booting
    - nano /etc/default/grub
    - Now add the line to the end of the file
      > GRUB_DISABLE_OS_PROBER=false
    - after saving the file run "grub-mkconfig -o /boot/grub/grub.cfg".
13. Now your OS can detect the dual-booting also.
14. Now we are done :
    - type exit and umount -R /mnt after type reboot.
    - Now your system will reboot and loads the Grub-Boot-Loader.

# BSPWM Configuration

1. First we have to install the bspwm.So we have to copy the folder in root directory to home
   - cp -r /Arch-Linux .
2. To do that go to Arch- Install directory and edit bspwm.sh file
   - cd Arch-Linux/Arch-Install
3. After editing the bspwm.sh file run that file giving the chmod +x permissions as above.
4. Now in the home/user directory of this repository there is a folder called .screenlayout and file called .xinitrc copy that to your home directory .
5. Now reboot the system.

# Login Manager Configuration

1. Now copy the files in usr directory in this repository to your respective directory.
2. Now open the lightdm file
   - sudo nano /etc/lightdm/lightdm.conf
3. And change the line as below under the [seats] section,
   - greeter-session=lightdm-webkit2-greeter
4. Save that file.
5. Now open the wekit2 file
   - sudo nano /etc/lightdm/lightdm-webkit2-greeter.conf
6. Change the debug line to true and wekit-theme to glorious.
7. Now save the file and exit.

8. for sddm copy the sddm.conf file from /etc folder of this repository and also install paru -S sddm-sugar-candy-git.




For qt5ct to work add QT_QPA_PLATFORMTHEME=qt5ct to /etc/environment file and source it
