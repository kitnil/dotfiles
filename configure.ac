AC_INIT([wigust dotfiles], [0.0.1], [go.wigust@gmail.com])

AC_PATH_PROG([GIT], [git])
if test "x$GIT" = "x"; then
  AC_MSG_ERROR([Git is missing; please install it.])
fi
AC_SUBST([GIT])

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([GUILE_MODULE_AVAILABLE])
m4_pattern_forbid([^GUILE_P])
m4_pattern_allow([^GUILE_PKG_ERRORS])
m4_pattern_forbid([^GUIX_])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
else
AC_SUBST([GUILE])
AC_CONFIG_FILES([src/guile/format-yaml/format-yaml], [chmod +x src/guile/format-yaml/format-yaml])
AC_CONFIG_FILES([src/guile/guix-guile-next-compile-file/guix-guile-next-compile-file], [chmod +x src/guile/guix-guile-next-compile-file/guix-guile-next-compile-file])
fi

AC_PATH_PROG([GUIX], [guix])
AC_SUBST([GUIX])

AC_PATH_PROG([GPG], [gpg])
AC_SUBST([GPG])

AC_PATH_PROG([GIT], [git])
AC_SUBST([GIT])

dnl Emacs (optional), for 'src/emacs/benchmark/benchmark.el'.
AC_PATH_PROG([EMACS], [emacs])
if test "x$EMACS" = x; then
    AC_MSG_WARN([Please install GNU Emacs to use src/emacs/benchmark/benchmark.el.])
else
    AC_SUBST([EMACS])
    AC_CONFIG_FILES([src/emacs/benchmark/benchmark.el], [chmod +x src/emacs/benchmark/benchmark.el])
fi

AC_PATH_PROG([SKOPEO], [skopeo])
AC_SUBST([SKOPEO])

AC_PATH_PROG([YAMLFMT], [yamlfmt])
AC_SUBST([YAMLFMT])

AC_PATH_PROG([YQ], [yq])
AC_SUBST([YQ])

AC_CONFIG_FILES([src/bash/guile-clean-modules/guile-clean-modules.sh], [chmod +x src/bash/guile-clean-modules/guile-clean-modules.sh])

AM_INIT_AUTOMAKE([1.14 -Woverride -Wno-portability])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
