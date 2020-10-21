#!/bin/sh

PATH=/run/current-system/profile/bin:$PATH

# Maximum Git repository size.
size=$((1024 * 1024 * 100)) # 100 MB

echo "#!/bin/sh"
for file in /home/oleg/majordomo/*/*
do
    if [ -d $file ] && [[ $(du -s "$file" | cut -f 1) -lt $size ]]
    then
        echo mkdir -p $(dirname $file)
        repository="git clone $(printf "%s $file\n" "$(git -C "$file" remote get-url origin 2>/dev/null)")"
        echo $repository
    fi
done

