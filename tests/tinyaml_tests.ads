--  Root test package for TinyAML
--  Provides the main test suite that aggregates all sub-suites

with AUnit.Test_Suites;

package Tinyaml_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Tinyaml_Tests;
