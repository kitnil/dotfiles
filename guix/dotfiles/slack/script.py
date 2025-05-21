#!/usr/bin/env python

"""Backup user's channel messages in a file, and delete them in Slack."""

from datetime import datetime, timedelta
import json
import os

from slack_cleaner2 import SlackCleaner, match


def main():
    """Entrypoint."""
    slack_cleaner = SlackCleaner("")
    # list of users
    slack_cleaner.users
    # list of all kind of channels
    slack_cleaner.conversations

    with open(os.environ["SLACK_BACKUP_FILE_NAME"], "a") as backup:
        # delete all messages in -bots channels
        for msg in slack_cleaner.msgs(
            filter(
                match(os.environ["SLACK_CHANNEL_NAME"]),
                slack_cleaner.conversations,
            )
        ):
            # delete messages, its files, and all its replies (thread)
            if msg.user_id == os.environ["SLACK_USER_ID"]:
                if (
                    datetime.now()
                    - timedelta(
                        hours=int(os.environ["SLACK_SAVE_HISTORY_UNTIL_HOURS"])
                    )
                ) > msg.dt:
                    backup.write(json.dumps(msg.json))
                    backup.write("\n")
                    msg.delete(replies=True, files=True)


if __name__ == "__main__":
    main()
