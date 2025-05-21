container_registry=harbor.home.wugi.info
.ONESHELL:
archlinux:
	set -o nounset -o errexit -o pipefail -o xtrace
	IMG="$(container_registry)/library/$@:$$(git rev-parse --abbrev-ref HEAD)-$$(git rev-parse HEAD | cut -c -8)-$$(date +%s)"
	sed -i "s/--destination=/--destination=$$(IMG)/" job.yaml
	git commit -m "apps: base: kaniko-archlinux: Update to $$(IMG)"

.PHONY: archlinux
