``` shell
helm upgrade --create-namespace --namespace youtube-dl --set nodeSelector."kubernetes\\.io/hostname"="kube8" youtube-dl ./.
curl --request PUT --upload-file "${HOME}/youtube-dl.txt" http://youtube-dl.vki.intr
rsync --verbose --archive --progress --list-only rsync://youtube-dl.vki.intr/
rsync --remove-source-files --verbose --archive --progress rsync://youtube-dl.vki.intr/downloads/ .
```

Thanks:
- [python simple http server with upload & download](https://gist.github.com/darkr4y/761d7536100d2124f5d0db36d4890109)
