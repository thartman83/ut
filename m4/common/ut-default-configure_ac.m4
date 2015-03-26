dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.26)

m4_define([project_name`'_major_version], [0])
m4_define([project_name`'_minor_version], [1])
m4_define([project_name`'_version], \\
          [project_name`'_major_version.`'project_name`'_minor_version])

AC_INIT([project_name`'],[1.0])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([1.11 dist-bzip2])
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_SUBST(translit(project_name, `a-z', `A-Z')`'_MAJOR_VERSION, [project_name`'_major_version])
AC_SUBST(translit(project_name, `a-z', `A-Z')`'_MINOR_VERSION, [project_name`'_minor_version])
AC_SUBST(translit(project_name, `a-z', `A-Z')`'_VERSION, [project_name`'_version])

dnl Check for programs

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
