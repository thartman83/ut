license_info
`#'ifndef translit(test_suite_name, `a-z', `A-Z')TESTS_HH_
`#'define translit(test_suite_name, `a-z', `A-Z')TESTS_HH_
`#'include <cppunit/TestFixture.h>
`#'include <cppunit/extensions/HelperMacros.h>

class test_suite_name`'Tests : public CppUnit::TestFixture
{
   CPPUNIT_TEST_SUITE(test_suite_name`'Tests);
   CPPUNIT_TEST_SUITE_END();

public:
   void setup();
   void tearDown();

// BEGIN TESTS
// END TESTS
};

CPPUNIT_TEST_SUITE_REGISTRATION(test_suite_name`'Tests);

`#'endif /* translit(test_suite_name, `a-z', `A-Z')TESTS_HH_ */
