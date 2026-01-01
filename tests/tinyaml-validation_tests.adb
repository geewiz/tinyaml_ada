with AUnit.Assertions; use AUnit.Assertions;

with Tinyaml.Parser;
with Tinyaml.Nodes;        use Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar; use Tinyaml.Nodes.Scalar;
with Tinyaml.Schemas.Prelude; use Tinyaml.Schemas.Prelude;
with Tinyaml.Validation;   use Tinyaml.Validation;

package body Tinyaml.Validation_Tests is

   use AUnit.Test_Cases.Registration;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Validation");
   end Name;

   overriding procedure Register_Tests (T : in out Test) is
   begin
      Register_Routine (T, Test_Str_Schema'Access, "Str schema accepts string");
      Register_Routine (T, Test_Int_Schema'Access, "Int schema accepts integer");
      Register_Routine (T, Test_Int_Schema_Range'Access, "Int schema with range");
      Register_Routine
        (T, Test_Int_Schema_Rejects_String'Access,
         "Int schema rejects non-integer");
      Register_Routine
        (T, Test_Bool_Schema'Access, "Bool schema accepts boolean values");
      Register_Routine
        (T, Test_Seq_Schema'Access, "Seq schema validates sequence items");
      Register_Routine
        (T, Test_Map_Schema'Access, "Map schema validates fields");
      Register_Routine
        (T, Test_Map_Schema_Missing_Field'Access,
         "Map schema detects missing required field");
      Register_Routine
        (T, Test_Optional_Schema'Access, "Optional schema allows missing field");
      Register_Routine
        (T, Test_Validated_As_Integer'Access, "Validated scalar As_Integer");
      Register_Routine
        (T, Test_Validated_As_Boolean'Access, "Validated scalar As_Boolean");
      Register_Routine
        (T, Test_Parse_And_Validate'Access,
         "Parse_And_Validate convenience function");
      Register_Routine
        (T, Test_Error_Message_For_Range'Access,
         "Error message shows field path and constraint");
      Register_Routine
        (T, Test_Error_Message_For_Missing_Field'Access,
         "Error message for missing required field");
      Register_Routine
        (T, Test_Error_Path_For_Sequence'Access,
         "Error path for sequence item validation");
      Register_Routine
        (T, Test_Nested_Map_Schema'Access, "Nested map schema validation");
   end Register_Tests;

   procedure Test_Str_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc    : Node_Access;
      Schema : Str_Schema;
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse ("hello");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "String should be valid");
   end Test_Str_Schema;

   procedure Test_Int_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc    : Node_Access;
      Schema : Int_Schema;
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse ("42");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Integer should be valid");
   end Test_Int_Schema;

   procedure Test_Int_Schema_Range (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema     : constant Int_Schema :=
        (Constraint => (Min => 1, Max => 100));
      R1, R2, R3 : Validation_Result;
   begin
      R1 := Validate (Tinyaml.Parser.Parse ("50"), Schema);
      Assert (R1.Is_Valid, "50 should be in range 1-100");

      R2 := Validate (Tinyaml.Parser.Parse ("0"), Schema);
      Assert (not R2.Is_Valid, "0 should be out of range");

      R3 := Validate (Tinyaml.Parser.Parse ("101"), Schema);
      Assert (not R3.Is_Valid, "101 should be out of range");
   end Test_Int_Schema_Range;

   procedure Test_Int_Schema_Rejects_String (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc    : Node_Access;
      Schema : Int_Schema;
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse ("not a number");
      Result := Validate (Doc, Schema);
      Assert (not Result.Is_Valid, "Non-integer should be invalid");
   end Test_Int_Schema_Rejects_String;

   procedure Test_Bool_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema : Bool_Schema;
   begin
      Assert
        (Validate (Tinyaml.Parser.Parse ("true"), Schema).Is_Valid,
         "'true' should be valid");
      Assert
        (Validate (Tinyaml.Parser.Parse ("false"), Schema).Is_Valid,
         "'false' should be valid");
      Assert
        (Validate (Tinyaml.Parser.Parse ("yes"), Schema).Is_Valid,
         "'yes' should be valid");
      Assert
        (Validate (Tinyaml.Parser.Parse ("no"), Schema).Is_Valid,
         "'no' should be valid");
      Assert
        (Validate (Tinyaml.Parser.Parse ("on"), Schema).Is_Valid,
         "'on' should be valid");
      Assert
        (Validate (Tinyaml.Parser.Parse ("off"), Schema).Is_Valid,
         "'off' should be valid");
      Assert
        (not Validate (Tinyaml.Parser.Parse ("maybe"), Schema).Is_Valid,
         "'maybe' should be invalid");
   end Test_Bool_Schema;

   procedure Test_Seq_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input  : constant String :=
        "- apple" & ASCII.LF &
        "- banana";
      Doc    : Node_Access;
      Schema : constant Seq_Schema :=
        (Item => To_Seq_Item (Str_Schema'(null record)));
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Sequence of strings should be valid");
   end Test_Seq_Schema;

   procedure Test_Map_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input  : constant String :=
        "name: Alice" & ASCII.LF &
        "age: 30";
      Doc    : Node_Access;
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Schema.Str ("name");
      Schema.Int ("age");
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Map with valid fields should be valid");
   end Test_Map_Schema;

   procedure Test_Map_Schema_Missing_Field (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input  : constant String := "name: Alice";
      Doc    : Node_Access;
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Schema.Str ("name");
      Schema.Int ("age");
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (not Result.Is_Valid, "Missing required field should be invalid");
   end Test_Map_Schema_Missing_Field;

   procedure Test_Optional_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema : Map_Schema;
      R1, R2 : Validation_Result;
   begin
      Schema.Str ("name");
      Schema.Str ("nickname", Optional => True);

      R1 := Validate (Tinyaml.Parser.Parse ("name: Alice"), Schema);
      Assert (R1.Is_Valid, "Missing optional field should be valid");

      R2 :=
        Validate
          (Tinyaml.Parser.Parse
             ("name: Alice" & ASCII.LF & "nickname: Ali"),
           Schema);
      Assert (R2.Is_Valid, "Present optional field should be valid");
   end Test_Optional_Schema;

   procedure Test_Validated_As_Integer (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc    : Node_Access;
      Schema : Int_Schema;
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse ("42");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Should be valid");
      Assert (Scalar_Node (Result.Root.all).Value = "42", "Should be '42'");
   end Test_Validated_As_Integer;

   procedure Test_Validated_As_Boolean (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc    : Node_Access;
      Schema : Bool_Schema;
      Result : Validation_Result;
   begin
      Doc := Tinyaml.Parser.Parse ("yes");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Should be valid");
      Assert (Scalar_Node (Result.Root.all).Value = "yes", "Should be 'yes'");
   end Test_Validated_As_Boolean;

   procedure Test_Parse_And_Validate (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Schema.Str ("host");
      Schema.Int ("port", Constraint => (Min => 1, Max => 65535));
      Result :=
        Parse_And_Validate
          ("host: localhost" & ASCII.LF & "port: 8080", Schema);
      Assert (Result.Is_Valid, "Should be valid");
   end Test_Parse_And_Validate;

   procedure Test_Error_Message_For_Range (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Schema.Int ("port", Constraint => (Min => 1, Max => 65535));
      Result := Parse_And_Validate ("port: 99999", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      Assert (Result.Error_Path = "port", "Path should be 'port'");
      Assert (Result.Error_Message'Length > 0, "Should have error message");
   end Test_Error_Message_For_Range;

   procedure Test_Error_Message_For_Missing_Field
     (T : in out Test_Case'Class)
   is
      pragma Unreferenced (T);
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Schema.Str ("name");
      Schema.Str ("email");
      Result := Parse_And_Validate ("name: Alice", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      Assert (Result.Error_Message'Length > 0, "Should have error message");
   end Test_Error_Message_For_Missing_Field;

   procedure Test_Error_Path_For_Sequence (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Schema : constant Seq_Schema :=
        (Item =>
           To_Seq_Item (Int_Schema'(Constraint => (Min => 1, Max => 10))));
      Result : Validation_Result;
   begin
      Result :=
        Parse_And_Validate
          ("- 5" & ASCII.LF & "- 20" & ASCII.LF & "- 3", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      Assert (Result.Error_Path'Length > 0, "Should have error path");
   end Test_Error_Path_For_Sequence;

   procedure Test_Nested_Map_Schema (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Db_Schema     : Map_Schema;
      Server_Schema : Map_Schema;
      Result        : Validation_Result;
   begin
      Db_Schema.Str ("host");
      Db_Schema.Int ("port", Constraint => (Min => 1, Max => 65535));

      Server_Schema.Str ("name");
      Server_Schema.Map ("database", Db_Schema);

      Result :=
        Parse_And_Validate
          ("name: myserver" & ASCII.LF &
           "database:" & ASCII.LF &
           "  host: localhost" & ASCII.LF &
           "  port: 5432",
           Server_Schema);
      Assert (Result.Is_Valid, "Valid nested config should pass");

      Result :=
        Parse_And_Validate
          ("name: myserver" & ASCII.LF &
           "database:" & ASCII.LF &
           "  host: localhost" & ASCII.LF &
           "  port: 99999",
           Server_Schema);
      Assert (not Result.Is_Valid, "Invalid port should fail");
      Assert (Result.Error_Path'Length > 0, "Should have error path");
   end Test_Nested_Map_Schema;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (new Test);
      return Result;
   end Suite;

end Tinyaml.Validation_Tests;
