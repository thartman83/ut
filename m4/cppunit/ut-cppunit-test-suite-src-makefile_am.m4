AM_CPPFLAGS = -I`'project_dir/src -I/usr/local/include/
bin_PROGRAMS = test_suite`'Tests
test_suite`'_SOURCES = main.cc test_suite`'Tests.cc test_suite`'Tests.hh
test_suite`'_LDADD = -L/usr/local/lib/ -lcppunit -lcppunitsexpoutputter
