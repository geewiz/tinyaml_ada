with Tinyaml.Lexer_Tests;
with Tinyaml.Parser_Tests;
with Tinyaml.Validation_Tests;

package body Tinyaml_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Tinyaml.Lexer_Tests.Suite);
      Result.Add_Test (Tinyaml.Parser_Tests.Suite);
      Result.Add_Test (Tinyaml.Validation_Tests.Suite);
      return Result;
   end Suite;

end Tinyaml_Tests;
