#!/bin/sh

# Deploy to user@host Guix, user profile, system profile.

host=192.168.105.115
user=pyhalov

user_profile=$(readlink -f $HOME/.guix-profile)
system_profile=$(readlink /run/current-system)
pull=$(readlink $HOME/.config/guix/latest)
# clover=$(env GUILE_LOAD_PATH=$HOME/src/guix-wigust:$HOME/src/guix-packages:$GUILE_LOAD_PATH guix system build /home/natsu/dotfiles/fiore/magnolia.scm)

echo -e "Deploy to \"$user@$host\":
  pull:     $pull
  user:     $user_profile
  magnolia: $system_profile
"

  # clover:   $clover

guix copy --to=$user@$host:2222 $pull $user_profile $system_profile /gnu/store/gsy5r333s5h7n19ri2zv4amf764wfasz-system
