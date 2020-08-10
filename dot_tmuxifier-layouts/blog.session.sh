#!/bin/sh

session_root "$HOME/src/haunt-blog"
if initialize_session "blog"; then
    load_window "blog"
fi
finalize_and_go_to_session
