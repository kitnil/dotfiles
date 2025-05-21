AC_INIT([wigust dotfiles], [0.0.1], [go.wigust@gmail.com])

AC_PATH_PROG([GIT], [git])
if test "x$GIT" = "x"; then
  AC_MSG_ERROR([Git is missing; please install it.])
fi
AC_SUBST([GIT])

AC_PATH_PROG([GUIX], [guix])
AC_SUBST([GUIX])

AC_PATH_PROG([PASS], [pass])
AC_SUBST([PASS])

dnl Emacs (optional), for 'src/emacs/benchmark/benchmark.el'.
AC_PATH_PROG([EMACS], [emacs])
if test "x$EMACS" = x; then
    AC_MSG_WARN([Please install GNU Emacs to use src/emacs/benchmark/benchmark.el.])
else
    AC_SUBST([EMACS])
    AC_CONFIG_FILES([src/emacs/benchmark/benchmark.el], [chmod +x src/emacs/benchmark/benchmark.el])
fi

AM_INIT_AUTOMAKE([1.14 -Woverride -Wno-portability])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
