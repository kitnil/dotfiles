TESTS =					\
  tests/connect.bats				\
  tests/executables.bats			\
  tests/mail.bats

check:
	bats $(TESTS)

install:
	chezmoi apply
	update-desktop-database ~/.local/share/applications
