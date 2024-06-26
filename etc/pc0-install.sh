cfdisk /dev/nvme0n1
cryptsetup luksFormat -i 1 /dev/nvme0n1p2
cryptsetup open /dev/nvme0n1p2 crypt-guix
pvcreate /dev/mapper/crypt-guix
vgcreate vg0 /dev/mapper/crypt-guix
lvcreate -L 64G -n guixroot vg0
mkfs.ext4 -L guix-root /dev/vg0/guixroot
mount /dev/vg0/guixroot /mnt
mkfs.fat -n BOOT0 -F32 /dev/nvme0n1p1
mkdir -p /mnt/boot/efi/
mount /dev/nvme0n1p1 /mnt/boot/efi/
mkdir -p /mnt/etc/
cp /etc/configuration/bare-bones.scm /mnt/etc/config.scm
chmod u+w /mnt/etc/config.scm
nano /mnt/etc/config.scm
parted /dev/nvme0n1 set 1 esp on
herd start cow-store /mnt
