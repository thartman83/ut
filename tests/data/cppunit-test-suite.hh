LICENSE
#ifndef FOOTESTS_HH_
#define FOOTESTS_HH_
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class FooTests : public CppUnit::TestFixture
{
   CPPUNIT_TEST_SUITE(FooTests);
   CPPUNIT_TEST_SUITE_END();

public:
   void setup();
   void tearDown();

// BEGIN TESTS
// END TESTS
};

CPPUNIT_TEST_SUITE_REGISTRATION(FooTests);

#endif /* FOOTESTS_HH_ */
