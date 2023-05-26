# Build and deploy container

```
guix pack -f docker -S /sbin=sbin -S /bin=bin dante
docker load -i /gnu/store/...-dante-docker-pack.tar.gz
docker tag dante:latest docker-registry.wugi.info/networking/dante:latest
docker push docker-registry.wugi.info/networking/dante:latest
```
