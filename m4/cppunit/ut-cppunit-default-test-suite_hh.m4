include(`ut-cppunit-license.m4')
`#'ifndef upcase(test_name)TESTS_HH_
`#'define upcase(test_name)TESTS_HH_
`#'include <cppunit/TestFixture.h>
`#'include <cppunit/extensions/HelperMacros.h>

class test_name`'Tests : public CppUnit::TestFixture
{
\tCPPUNIT_TEST_SUITE(test_name`'Tests);
\tCPPUNIT_TEST_SUITE_END();

public:
\tvoid setup();
\tvoid tearDown();

};

CPPUNIT_TEST_SUITE_REGISTRATION(test_name`'Tests);

`#'endif /* upcase(test_name)TESTS_HH_ */
