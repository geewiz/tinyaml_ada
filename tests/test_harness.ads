--  Simple test harness for TinyAML tests

package Test_Harness is

   --  Initialize the test harness
   procedure Initialize (Suite_Name : String);

   --  Start a new test
   procedure Start_Test (Name : String);

   --  Assert that a condition is true
   procedure Assert (Condition : Boolean; Message : String := "");

   --  Assert that two strings are equal
   procedure Assert_Equal (Expected, Actual : String; Message : String := "");

   --  Assert that two integers are equal
   procedure Assert_Equal (Expected, Actual : Integer; Message : String := "");

   --  Mark current test as failed with a message
   procedure Fail (Message : String);

   --  Mark current test as passed (optional, auto-passed if no failures)
   procedure Pass;

   --  Print summary
   procedure Summary;

   --  Check if all tests passed
   function All_Passed return Boolean;

end Test_Harness;
