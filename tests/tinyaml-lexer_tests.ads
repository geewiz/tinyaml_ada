with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Suites;

package Tinyaml.Lexer_Tests is

   type Test is new Test_Case with null record;

   overriding function Name (T : Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   procedure Test_Empty_Input (T : in out Test_Case'Class);
   procedure Test_Simple_Scalar (T : in out Test_Case'Class);
   procedure Test_Quoted_String_Double (T : in out Test_Case'Class);
   procedure Test_Quoted_String_Single (T : in out Test_Case'Class);
   procedure Test_Escape_Sequences (T : in out Test_Case'Class);
   procedure Test_Colon (T : in out Test_Case'Class);
   procedure Test_Dash (T : in out Test_Case'Class);
   procedure Test_Indentation (T : in out Test_Case'Class);
   procedure Test_Dedent (T : in out Test_Case'Class);
   procedure Test_Comment (T : in out Test_Case'Class);
   procedure Test_Flow_Style_Rejected (T : in out Test_Case'Class);
   procedure Test_Anchor_Rejected (T : in out Test_Case'Class);
   procedure Test_Tag_Rejected (T : in out Test_Case'Class);

end Tinyaml.Lexer_Tests;
