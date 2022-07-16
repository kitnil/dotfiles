#!/usr/bin/env bash

git_https_repository_to_domain_name()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -oP 'https://(.*)/' \
        | sed 's@https://@@; s@/.*@@'
}

git_ssh_repository_to_domain_name()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -oP 'git@(.*):' \
        | sed 's/git@//; s/://'
}

for dir in */.git
do
    domain="$(git_https_repository_to_domain_name "$dir")"
    if [[ $domain != "" ]]
    then
        echo mkdir -p "$domain"
        echo sudo mv "$(dirname "$dir")" "$domain"
    fi
done

for dir in */.git
do
    domain="$(git_ssh_repository_to_domain_name "$dir")"
    if [[ $domain != "" ]]
    then
        echo mkdir -p "$domain"
        echo sudo mv "$(dirname "$dir")" "$domain"
    fi
done
