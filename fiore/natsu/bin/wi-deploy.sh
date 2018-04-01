#!/bin/sh

# Deploy to user@host Guix, user profile, system profile.

host=192.168.105.115
user=natsu

user_profile=$(readlink -f $HOME/.guix-profile)
system_profile=$(readlink /run/current-system)
pull=$(readlink $HOME/.config/guix/latest)

echo -e "Deploy to \"$user@$host\":
  $pull
  $user_profile
  $system_profile"

guix copy --to=$user@$host $pull $user_profile $system_profile
