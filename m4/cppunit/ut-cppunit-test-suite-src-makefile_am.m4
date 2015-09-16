AM_CPPFLAGS = -I$(top_builddir)/src -I/usr/local/include/

bin_PROGRAMS = test_suite`'Tests
test_suite`'Tests_SOURCES = main.cc test_suite`'Tests.cc $(top_builddir)/src/test_suite.cc
test_suite`'Tests_LDADD = -L/usr/local/lib/ -lcppunit -lcppunitsexpoutputter
