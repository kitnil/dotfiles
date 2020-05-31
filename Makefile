TESTS =						\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/guix.bats				\
  tests/mail.bats

clean:
	rm -rf test-tmp

check:
	bats $(TESTS)

install:
	chezmoi apply
	update-desktop-database ~/.local/share/applications
