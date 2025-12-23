with Test_Harness;
with Tinyaml.Parser;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Scalar;
with Tinyaml.Schemas.Prelude;
with Tinyaml.Validation;

package body Test_Validation is

   use Test_Harness;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Scalar;
   use Tinyaml.Schemas.Prelude;
   use Tinyaml.Validation;

   procedure Test_Str_Schema is
      Doc    : Node_Access;
      Schema : Str_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Str schema accepts string");
      Doc := Tinyaml.Parser.Parse ("hello");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "String should be valid");
      Pass;
   end Test_Str_Schema;

   procedure Test_Int_Schema is
      Doc     : Node_Access;
      Schema  : Int_Schema;
      Result  : Validation_Result;
   begin
      Start_Test ("Int schema accepts integer");
      Doc := Tinyaml.Parser.Parse ("42");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Integer should be valid");
      Pass;
   end Test_Int_Schema;

   procedure Test_Int_Schema_Range is
      Schema : constant Int_Schema :=
        (Constraint => (Min => 1, Max => 100));
      R1, R2, R3 : Validation_Result;
   begin
      Start_Test ("Int schema with range");
      R1 := Validate (Tinyaml.Parser.Parse ("50"), Schema);
      Assert (R1.Is_Valid, "50 should be in range 1-100");

      R2 := Validate (Tinyaml.Parser.Parse ("0"), Schema);
      Assert (not R2.Is_Valid, "0 should be out of range");

      R3 := Validate (Tinyaml.Parser.Parse ("101"), Schema);
      Assert (not R3.Is_Valid, "101 should be out of range");
      Pass;
   end Test_Int_Schema_Range;

   procedure Test_Int_Schema_Rejects_String is
      Doc    : Node_Access;
      Schema : Int_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Int schema rejects non-integer");
      Doc := Tinyaml.Parser.Parse ("not a number");
      Result := Validate (Doc, Schema);
      Assert (not Result.Is_Valid, "Non-integer should be invalid");
      Pass;
   end Test_Int_Schema_Rejects_String;

   procedure Test_Bool_Schema is
      Schema : Bool_Schema;
   begin
      Start_Test ("Bool schema accepts boolean values");
      Assert (Validate (Tinyaml.Parser.Parse ("true"), Schema).Is_Valid);
      Assert (Validate (Tinyaml.Parser.Parse ("false"), Schema).Is_Valid);
      Assert (Validate (Tinyaml.Parser.Parse ("yes"), Schema).Is_Valid);
      Assert (Validate (Tinyaml.Parser.Parse ("no"), Schema).Is_Valid);
      Assert (Validate (Tinyaml.Parser.Parse ("on"), Schema).Is_Valid);
      Assert (Validate (Tinyaml.Parser.Parse ("off"), Schema).Is_Valid);
      Assert (not Validate (Tinyaml.Parser.Parse ("maybe"), Schema).Is_Valid);
      Pass;
   end Test_Bool_Schema;

   procedure Test_Seq_Schema is
      Input  : constant String :=
        "- apple" & ASCII.LF &
        "- banana";
      Doc    : Node_Access;
      Schema : constant Seq_Schema :=
        (Item => To_Seq_Item (Str_Schema'(null record)));
      Result : Validation_Result;
   begin
      Start_Test ("Seq schema validates sequence items");
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Sequence of strings should be valid");
      Pass;
   end Test_Seq_Schema;

   procedure Test_Map_Schema is
      Input  : constant String :=
        "name: Alice" & ASCII.LF &
        "age: 30";
      Doc    : Node_Access;
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Map schema validates fields");
      Schema.Str ("name");
      Schema.Int ("age");
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Map with valid fields should be valid");
      Pass;
   end Test_Map_Schema;

   procedure Test_Map_Schema_Missing_Field is
      Input  : constant String := "name: Alice";
      Doc    : Node_Access;
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Map schema detects missing required field");
      Schema.Str ("name");
      Schema.Int ("age");
      Doc := Tinyaml.Parser.Parse (Input);
      Result := Validate (Doc, Schema);
      Assert (not Result.Is_Valid, "Missing required field should be invalid");
      Pass;
   end Test_Map_Schema_Missing_Field;

   procedure Test_Optional_Schema is
      Schema : Map_Schema;
      R1, R2 : Validation_Result;
   begin
      Start_Test ("Optional schema allows missing field");
      Schema.Str ("name");
      Schema.Str ("nickname", Optional => True);

      R1 := Validate (Tinyaml.Parser.Parse ("name: Alice"), Schema);
      Assert (R1.Is_Valid, "Missing optional field should be valid");

      R2 := Validate (Tinyaml.Parser.Parse (
        "name: Alice" & ASCII.LF & "nickname: Ali"), Schema);
      Assert (R2.Is_Valid, "Present optional field should be valid");
      Pass;
   end Test_Optional_Schema;

   procedure Test_Validated_As_Integer is
      Doc    : Node_Access;
      Schema : Int_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Validated scalar As_Integer");
      Doc := Tinyaml.Parser.Parse ("42");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Should be valid");
      --  Verify the value is correct
      Assert_Equal ("42", Scalar_Node (Result.Root.all).Value);
      Pass;
   end Test_Validated_As_Integer;

   procedure Test_Validated_As_Boolean is
      Doc    : Node_Access;
      Schema : Bool_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Validated scalar As_Boolean");
      Doc := Tinyaml.Parser.Parse ("yes");
      Result := Validate (Doc, Schema);
      Assert (Result.Is_Valid, "Should be valid");
      --  Verify the value is correct (accepted as boolean)
      Assert_Equal ("yes", Scalar_Node (Result.Root.all).Value);
      Pass;
   end Test_Validated_As_Boolean;

   procedure Test_Parse_And_Validate is
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Parse_And_Validate convenience function");
      Schema.Str ("host");
      Schema.Int ("port", Constraint => (Min => 1, Max => 65535));
      Result := Parse_And_Validate (
        "host: localhost" & ASCII.LF & "port: 8080", Schema);
      Assert (Result.Is_Valid, "Should be valid");
      Pass;
   end Test_Parse_And_Validate;

   procedure Test_Error_Message_For_Range is
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Error message shows field path and constraint");
      Schema.Int ("port", Constraint => (Min => 1, Max => 65535));
      Result := Parse_And_Validate ("port: 99999", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      Assert (Result.Error_Path = "port", "Path should be 'port'");
      Assert (Result.Error_Message'Length > 0, "Should have error message");
      Pass;
   end Test_Error_Message_For_Range;

   procedure Test_Error_Message_For_Missing_Field is
      Schema : Map_Schema;
      Result : Validation_Result;
   begin
      Start_Test ("Error message for missing required field");
      Schema.Str ("name");
      Schema.Str ("email");
      Result := Parse_And_Validate ("name: Alice", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      Assert (Result.Error_Message'Length > 0, "Should have error message");
      Pass;
   end Test_Error_Message_For_Missing_Field;

   procedure Test_Error_Path_For_Sequence is
      Schema : constant Seq_Schema :=
        (Item => To_Seq_Item (Int_Schema'(Constraint => (Min => 1, Max => 10))));
      Result : Validation_Result;
   begin
      Start_Test ("Error path for sequence item validation");
      Result := Parse_And_Validate (
        "- 5" & ASCII.LF & "- 20" & ASCII.LF & "- 3", Schema);
      Assert (not Result.Is_Valid, "Should be invalid");
      --  Path should indicate the sequence index
      Assert (Result.Error_Path'Length > 0, "Should have error path");
      Pass;
   end Test_Error_Path_For_Sequence;

   procedure Test_Nested_Map_Schema is
      Db_Schema     : Map_Schema;
      Server_Schema : Map_Schema;
      Result        : Validation_Result;
   begin
      Start_Test ("Nested map schema validation");
      --  Build nested schema first
      Db_Schema.Str ("host");
      Db_Schema.Int ("port", Constraint => (Min => 1, Max => 65535));

      --  Add it to parent
      Server_Schema.Str ("name");
      Server_Schema.Map ("database", Db_Schema);

      --  Valid config
      Result := Parse_And_Validate (
        "name: myserver" & ASCII.LF &
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 5432", Server_Schema);
      Assert (Result.Is_Valid, "Valid nested config should pass");

      --  Invalid config (port out of range)
      Result := Parse_And_Validate (
        "name: myserver" & ASCII.LF &
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 99999", Server_Schema);
      Assert (not Result.Is_Valid, "Invalid port should fail");
      Assert (Result.Error_Path'Length > 0, "Should have error path");
      Pass;
   end Test_Nested_Map_Schema;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Initialize ("Schema and Validation Tests");

      Test_Str_Schema;
      Test_Int_Schema;
      Test_Int_Schema_Range;
      Test_Int_Schema_Rejects_String;
      Test_Bool_Schema;
      Test_Seq_Schema;
      Test_Map_Schema;
      Test_Map_Schema_Missing_Field;
      Test_Optional_Schema;
      Test_Validated_As_Integer;
      Test_Validated_As_Boolean;
      Test_Parse_And_Validate;
      Test_Error_Message_For_Range;
      Test_Error_Message_For_Missing_Field;
      Test_Error_Path_For_Sequence;
      Test_Nested_Map_Schema;
   end Run_Tests;

end Test_Validation;
