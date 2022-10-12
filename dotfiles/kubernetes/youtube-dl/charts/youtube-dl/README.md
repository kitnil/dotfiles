``` shell
helm install --create-namespace --namespace youtube-dl --set nodeSelector."kubernetes\\.io/hostname"="kube8" youtube-dl ./.
curl -X PUT --upload-file ~/.bashrc http://kube8.intr:30080/bashrc
rsync --verbose --archive --progress --list-only rsync://kube8.intr:30873/
rsync --remove-source-files --verbose --archive --progress rsync://kube8.intr:30873/downloads/ .
```

Thanks:
- [python simple http server with upload & download](https://gist.github.com/darkr4y/761d7536100d2124f5d0db36d4890109)
