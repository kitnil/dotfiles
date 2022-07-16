#!/usr/bin/env bash

git_https_repository_to_domain_name()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -oP 'https://(.*)/' \
        | sed 's@https://@@' \
        | cut -d/ -f 2
}

git_https_repository_to_project()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -o 'https://.* ' \
        | cut -d/ -f 5 \
        | sed 's@\s\(fetch\)@@;' \
        | sed 's@.git@@'
}

git_ssh_repository_to_domain_name()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -o 'git@.*' \
        | cut -d: -f 2 \
        | sed 's@ (fetch)@@' \
        | sed 's@.git@@' \
        | cut -d/ -f 1
}

git_ssh_repository_to_project()
{
    echo "$(git -C "$(dirname "$1")" remote -v)" \
        | grep origin \
        | head -n 1 \
        | grep -o 'git@.*' \
        | cut -d/ -f 2 \
        | sed 's@ (fetch)@@' \
        | sed 's@.git@@'
}

for dir in */.git
do
    domain="$(git_https_repository_to_domain_name "$dir")"
    project="$(git_https_repository_to_project "$dir")"
    if [[ $domain != "" ]]
    then
        if [[ $domain -eq $project ]]
        then
            echo sudo mv -i "$(dirname "$dir")" "$(dirname "$dir").1"
            echo mkdir -p "$domain"
            echo sudo mv -i "$(dirname "$dir").1" "${domain}/${project}"
        else
            echo mkdir -p "$domain"
            echo sudo mv -i "$(dirname "$dir")" "${domain}/${project}"
        fi
    fi
done

for dir in */.git
do
    domain="$(git_ssh_repository_to_domain_name "$dir")"
    project="$(git_ssh_repository_to_project "$dir")"
    if [[ $domain != "" ]]
    then
        if [[ $domain -eq $project ]]
        then
            echo sudo mv -i "$(dirname "$dir")" "$(dirname "$dir").1"
            echo mkdir -p "$domain"
            echo sudo mv -i "$(dirname "$dir").1" "${domain}/${project}"
        else
            echo mkdir -p "$domain"
            echo sudo mv -i "$(dirname "$dir")" "${domain}/${project}"
        fi
    fi
done

# oleg@guixsd ~/src$ for dir in $(ls -d */*/.git | xargs dirname | xargs dirname | sort -u); do (cd "$dir"; eval "$(~/.local/share/chezmoi/dot_local/bin/executable_git_directories_to_domain.sh)"); done
