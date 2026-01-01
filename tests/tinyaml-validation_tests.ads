with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Suites;

package Tinyaml.Validation_Tests is

   type Test is new Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   procedure Test_Str_Schema (T : in out Test_Case'Class);
   procedure Test_Int_Schema (T : in out Test_Case'Class);
   procedure Test_Int_Schema_Range (T : in out Test_Case'Class);
   procedure Test_Int_Schema_Rejects_String (T : in out Test_Case'Class);
   procedure Test_Bool_Schema (T : in out Test_Case'Class);
   procedure Test_Seq_Schema (T : in out Test_Case'Class);
   procedure Test_Map_Schema (T : in out Test_Case'Class);
   procedure Test_Map_Schema_Missing_Field (T : in out Test_Case'Class);
   procedure Test_Optional_Schema (T : in out Test_Case'Class);
   procedure Test_Validated_As_Integer (T : in out Test_Case'Class);
   procedure Test_Validated_As_Boolean (T : in out Test_Case'Class);
   procedure Test_Parse_And_Validate (T : in out Test_Case'Class);
   procedure Test_Error_Message_For_Range (T : in out Test_Case'Class);
   procedure Test_Error_Message_For_Missing_Field (T : in out Test_Case'Class);
   procedure Test_Error_Path_For_Sequence (T : in out Test_Case'Class);
   procedure Test_Nested_Map_Schema (T : in out Test_Case'Class);

end Tinyaml.Validation_Tests;
