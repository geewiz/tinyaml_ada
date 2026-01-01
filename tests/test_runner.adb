--  Main test runner for TinyAML tests

with AUnit.Reporter.Text;
with AUnit.Run;

with Tinyaml_Tests;

procedure Test_Runner is
   procedure Run is new AUnit.Run.Test_Runner (Tinyaml_Tests.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test_Runner;
