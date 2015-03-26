`dnl Process this file with autoconf to produce a configure script.'

AC_PREREQ(2.26)

m4_define([project_name`'_major_version], [0])
m4_define([project_name`'_minor_version], [1])
m4_define([project_name`'_version], \
          [project_name`'_major_version.`'project_name`'_minor_version])

AC_INIT([project_name`'],[project_name`'_version])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([1.11 dist-bzip2])
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_SUBST(translit(project_name, `a-z', `A-Z')`'_MAJOR_VERSION, [project_name`'_major_version])
AC_SUBST(translit(project_name, `a-z', `A-Z')`'_MINOR_VERSION, [project_name`'_minor_version])
AC_SUBST(translit(project_name, `a-z', `A-Z')`'_VERSION, [project_name`'_version])

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CXX
AC_LANG(C++)
AC_PROG_LIBTOOL
AC_LTDL_DLLIB
PKG_CHECK_MODULES([CPPUNIT], [cppunit])

# Check for standard libraries
AC_CXX_HAVE_STD
AC_CXX_HAVE_STL

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile]
AC_CONFIG_FILES([tests/Makefile])

AC_OUTPUT
