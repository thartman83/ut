LICENSE
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunitsexpoutputter/SexpOutputter.h>
#include "fooTests.hh"

int main(int argc, char *argv[])
{
   CppUnit::TestResult controller;

   CppUnit::TestResultCollector result;
   controller.addListener(&result);

   CppUnit::TestRunner runner;
   runner.addTest(CppUnit::TestFactoryRegistry::getRegistry().makeTest());
   
   try {
      runner.run(controller);
      CppUnit::SexpOutputter outputter(&result, std::cout);
   } catch(...) {
   }
   
   return (result.wasSuccessful() ? 0 : 1);
}
