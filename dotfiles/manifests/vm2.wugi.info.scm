(use-modules (gnu packages))

(specifications->manifest
  (map symbol->string
       '(make restic python curl ffmpeg git tmux screen rsync openjdk@12)))
