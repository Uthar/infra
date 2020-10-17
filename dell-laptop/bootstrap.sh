#!/bin/sh

# Bootstrap a machine from a freshly booted NixOS ISO
# Run the script as root or with sudo
# Assumes /dev/sda as the disk to install on

set -e

warn() {
cat <<EOF
===============
NixOS installer
===============

By continuing, I will PERMANENTLY DELETE ALL DATA ON /dev/sda

To continue, write uppercase 'WIPE MY DISK':
EOF

read -r yes
if ! [ "$yes" == "WIPE MY DISK" ]; then
    echo "Not continuing - exiting."
    exit 1
fi
}

askpassword() {
    stty -echo
    printf "Password for encrypted disk: "
    read -r PASSWORD
    printf "\nRepeat password: "
    read -r PASSWORDD
    if [ "$PASSWORD" != "$PASSWORDD" ]; then
        printf "\nPasswords do not match.\n\n";
        askpassword;
    fi
    stty echo
    printf "\n"
    unset PASSWORDD
}

partition() {
    parted /dev/sda -- mklabel msdos
    parted /dev/sda -- mkpart primary 1MiB -1
}

genkey() {
    dd if=/dev/random of=/root/key bs=512 count=16384 iflag=fullblock
    chmod 0400 /root/key
}

luksformat() {

    echo -n $PASSWORD | cryptsetup --cipher aes-xts-plain64  \
               --key-size 512 \
               --key-file=- \
               --hash whirlpool \
               --use-random \
               --type luks1 \
               luksFormat /dev/sda1

    echo -n $PASSWORD | cryptsetup -q  \
               --key-file=-  \
               luksAddKey /dev/sda1 /root/key
}

luksopen() {
    echo -n $PASSWORD | cryptsetup open --key-file=- /dev/sda1 encrypted
}

dolvm() {
    pvcreate /dev/mapper/encrypted
    vgcreate nixos /dev/mapper/encrypted
    lvcreate -L 8G nixos -n swap /dev/mapper/encrypted
    lvcreate -l +100%FREE nixos -n main /dev/mapper/encrypted
}

domkfs() {
    mkfs.ext4 /dev/nixos/main
    e2label /dev/nixos/main "main"
    mkswap /dev/nixos/swap
    swaplabel -L "swap" /dev/nixos/swap
}

domount() {
    mount /dev/nixos/main /mnt
    swapon /dev/nixos/swap
}

doinitrd() {
    mkdir -pv /mnt/root
    find /root/key -print0 | cpio --null --create --verbose --format=newc | gzip --best > /mnt/root/extra-initramfs.cpio.gz
    chmod 0400 /mnt/root/extra-initramfs.cpio.gz
}

cpconfig() {
    mkdir -pv /mnt/etc/nixos/
    cp -v ./* /mnt/etc/nixos/
}

# Get confirmation for destroying data on current disk 
warn

# Fetch password for the LUKS device from stdin
askpassword

# Create system partitions to create the LUKS device on
partition

# Generate a key for the LUKS device from /dev/random
genkey

# Create the LUKS device using password and key
luksformat

# Open the LUKS device
luksopen

# Password is not needed further on
unset PASSWORD

# Create logical volumes on the LUKS device
dolvm

# Create filesystems on the logical volumes
domkfs

# Mount the filesystems to install NixOS on them
domount

# Create a supplementary initrd cpio image on the mounted fs
# The image contains the LUKS key to unlock the device during stage 1
# NixOS configuration assumes the existence of this initrd image
doinitrd

# Copy NixOS configuration to the mounted fs
cpconfig

# Install NixOS using the copied over configuration
nixos-install --no-root-passwd

echo "NixOS is installed. You can reboot into the system."
