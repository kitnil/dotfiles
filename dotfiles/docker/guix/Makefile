container_registry=harbor.home.wugi.info
.ONESHELL:
guix-image-workstation:
	set -o nounset -o errexit -o pipefail -o xtrace
	IMG=harbor.home.wugi.info/library/guix-image-workstation:e5706265
	export IMG
	commit_8=$$(git rev-parse HEAD | cut -c -8)
	./build.sh
	docker build --network=host --pull --tag "$(container_registry)/library/$@:$$commit_8" .
	docker push "$(container_registry)/library/$@:$$commit_8"
	echo "docker://$(container_registry)/library/$@:$$commit_8"
