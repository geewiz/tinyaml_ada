--  Main test runner for TinyAML tests

with Test_Harness;
with Test_Lexer;
with Test_Parser;
with Test_Validation;

with Ada.Command_Line;

procedure Test_Runner is
begin
   --  Run all test suites
   Test_Lexer.Run_Tests;
   Test_Parser.Run_Tests;
   Test_Validation.Run_Tests;

   --  Print summary and set exit code
   Test_Harness.Summary;

   if Test_Harness.All_Passed then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Runner;
