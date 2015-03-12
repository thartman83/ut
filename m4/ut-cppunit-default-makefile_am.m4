AM_CPPFLAGS = -I`'project-dir/src -I/usr/local/include/
bin_PROGRAMS = test_name`'Tests
test_name`'_SOURCES = main.cc test_name`'Tests.cc test_name`'Tests.hh
test_name`'_LDADD = -L/usr/local/lib/ -lcppunit -lcppunitsexpoutputter
