container_registry=harbor.home.wugi.info
.ONESHELL:
archlinux-systemd:
	set -o nounset -o errexit -o pipefail -o xtrace
	IMG="$(container_registry)/library/$@:$$(git rev-parse --abbrev-ref HEAD)-$$(git rev-parse HEAD | cut -c -8)-$$(date +%s)"
	docker build --network=host --pull --tag "$$IMG"
	docker push "$$IMG"
	echo "$$IMG"
