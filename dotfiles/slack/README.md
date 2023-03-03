# Build container

``` shell
docker build --tag slack-cleaner2 .
```

# Set environment variables

```
SLACK_USER_ID="XXXXXXXXXXX"
SLACK_CHANNEL_NAME="XXXXXXX"
SLACK_BACKUP_FILE_NAME="XXXXXX.txt"
SLACK_SAVE_HISTORY_UNTIL_HOURS=8
```

# Run container

``` shell
docker run \
       --env SLACK_USER_ID="$SLACK_USER_ID" \
       --env SLACK_CHANNEL_NAME="$SLACK_CHANNEL_NAME" \
       --env SLACK_BACKUP_FILE_NAME="$SLACK_BACKUP_FILE_NAME" \
       --env SLACK_SAVE_HISTORY_UNTIL_HOURS="$SLACK_SAVE_HISTORY_UNTIL_HOURS" \
       --volume "${HOME}/slack_cleaner2:/workdir" \
       --detach \
       --name slack-cleaner2 \
       slack-cleaner2
```
