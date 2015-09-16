license_info
`#'ifndef translit(test_suite, `a-z', `A-Z')TESTS_HH_
`#'define translit(test_suite, `a-z', `A-Z')TESTS_HH_
`#'include <cppunit/TestFixture.h>
`#'include <cppunit/extensions/HelperMacros.h>

class test_suite`'Tests : public CppUnit::TestFixture
{
   CPPUNIT_TEST_SUITE(test_suite`'Tests);
   CPPUNIT_TEST_SUITE_END();

public:
   void setup();
   void tearDown();

};

CPPUNIT_TEST_SUITE_REGISTRATION(test_suite`'Tests);

`#'endif /* translit(test_suite, `a-z', `A-Z')TESTS_HH_ */
