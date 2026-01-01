with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Suites;

package Tinyaml.Parser_Tests is

   type Test is new Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   procedure Test_Empty_Input (T : in out Test_Case'Class);
   procedure Test_Simple_Scalar (T : in out Test_Case'Class);
   procedure Test_Simple_Mapping (T : in out Test_Case'Class);
   procedure Test_Nested_Mapping (T : in out Test_Case'Class);
   procedure Test_Simple_Sequence (T : in out Test_Case'Class);
   procedure Test_Sequence_Of_Mappings (T : in out Test_Case'Class);
   procedure Test_Mapping_With_Sequence (T : in out Test_Case'Class);
   procedure Test_Duplicate_Key_Rejected (T : in out Test_Case'Class);
   procedure Test_Navigate_Path (T : in out Test_Case'Class);
   procedure Test_Get_String (T : in out Test_Case'Class);
   procedure Test_Parse_Document (T : in out Test_Case'Class);
   procedure Test_Free_Node (T : in out Test_Case'Class);

end Tinyaml.Parser_Tests;
