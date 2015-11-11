AM_CPPFLAGS = -I$(top_builddir)/src -I/usr/local/include/

bin_PROGRAMS = test_suite_name`'Tests
test_suite_name`'Tests_SOURCES = main.cc test_suite_name`'Tests.cc $(top_builddir)/src/test_suite_name.cc
test_suite_name`'Tests_LDADD = -L/usr/local/lib/ -lcppunit -lcppunitsexpoutputter
