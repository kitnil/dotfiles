docker container create harbor.home.wugi.info/library/guix-image-builder:88083b51
docker export a2e3c388c22b0f6696e4a31fd85b3ea0f5a7b185bc11574d9490488b82a9a2b4 | sudo tar -C rootfs -x
