with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Test_Harness is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   --  Test state
   Current_Suite  : Unbounded_String;
   Current_Test   : Unbounded_String;
   Test_Passed    : Boolean := True;
   Total_Tests    : Natural := 0;
   Passed_Tests   : Natural := 0;
   Failed_Tests   : Natural := 0;
   In_Test        : Boolean := False;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Suite_Name : String) is
   begin
      Current_Suite := To_Unbounded_String (Suite_Name);
      Total_Tests := 0;
      Passed_Tests := 0;
      Failed_Tests := 0;
      In_Test := False;

      Put_Line ("========================================");
      Put_Line ("Test Suite: " & Suite_Name);
      Put_Line ("========================================");
      New_Line;
   end Initialize;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (Name : String) is
   begin
      --  Finish previous test if any
      if In_Test then
         if Test_Passed then
            Passed_Tests := Passed_Tests + 1;
         end if;
      end if;

      Current_Test := To_Unbounded_String (Name);
      Test_Passed := True;
      In_Test := True;
      Total_Tests := Total_Tests + 1;

      Put ("  [    ] " & Name);
   end Start_Test;

   ------------
   -- Assert --
   ------------

   procedure Assert (Condition : Boolean; Message : String := "") is
   begin
      if not Condition then
         Fail (if Message = "" then "Assertion failed" else Message);
      end if;
   end Assert;

   ------------------
   -- Assert_Equal --
   ------------------

   procedure Assert_Equal (Expected, Actual : String; Message : String := "")
   is
   begin
      if Expected /= Actual then
         Fail ((if Message = "" then "" else Message & ": ") &
               "expected '" & Expected & "', got '" & Actual & "'");
      end if;
   end Assert_Equal;

   ------------------
   -- Assert_Equal --
   ------------------

   procedure Assert_Equal (Expected, Actual : Integer; Message : String := "")
   is
   begin
      if Expected /= Actual then
         Fail ((if Message = "" then "" else Message & ": ") &
               "expected" & Expected'Image & ", got" & Actual'Image);
      end if;
   end Assert_Equal;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String) is
   begin
      if Test_Passed then
         --  First failure for this test
         Test_Passed := False;
         Failed_Tests := Failed_Tests + 1;
         --  Overwrite the [    ] with [FAIL]
         Put (ASCII.CR & "  [FAIL] " & To_String (Current_Test));
         New_Line;
      end if;
      Put_Line ("         " & Message);
   end Fail;

   ----------
   -- Pass --
   ----------

   procedure Pass is
   begin
      if Test_Passed and In_Test then
         --  Overwrite the [    ] with [ OK ]
         Put (ASCII.CR & "  [ OK ] " & To_String (Current_Test));
         New_Line;
      end if;
   end Pass;

   -------------
   -- Summary --
   -------------

   procedure Summary is
   begin
      --  Finish final test if any
      if In_Test then
         if Test_Passed then
            Passed_Tests := Passed_Tests + 1;
            Pass;
         end if;
         In_Test := False;
      end if;

      New_Line;
      Put_Line ("========================================");
      Put_Line ("Results:" & Total_Tests'Image & " tests," &
                Passed_Tests'Image & " passed," &
                Failed_Tests'Image & " failed");
      Put_Line ("========================================");
   end Summary;

   ----------------
   -- All_Passed --
   ----------------

   function All_Passed return Boolean is
   begin
      return Failed_Tests = 0;
   end All_Passed;

end Test_Harness;
