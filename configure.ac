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

AM_INIT_AUTOMAKE([1.14 -Woverride -Wno-portability])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
