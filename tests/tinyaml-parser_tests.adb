with AUnit.Assertions; use AUnit.Assertions;

with Tinyaml.Documents;  use Tinyaml.Documents;
with Tinyaml.Parser;
with Tinyaml.Nodes;      use Tinyaml.Nodes;
with Tinyaml.Nodes.Map;  use Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Navigation; use Tinyaml.Nodes.Navigation;
with Tinyaml.Nodes.Scalar;     use Tinyaml.Nodes.Scalar;
with Tinyaml.Nodes.Sequence;   use Tinyaml.Nodes.Sequence;

package body Tinyaml.Parser_Tests is

   use AUnit.Test_Cases.Registration;

   overriding function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Parser");
   end Name;

   overriding procedure Register_Tests (T : in out Test) is
   begin
      Register_Routine (T, Test_Empty_Input'Access, "Empty input");
      Register_Routine (T, Test_Simple_Scalar'Access, "Simple scalar document");
      Register_Routine (T, Test_Simple_Mapping'Access, "Simple mapping");
      Register_Routine (T, Test_Nested_Mapping'Access, "Nested mapping");
      Register_Routine (T, Test_Simple_Sequence'Access, "Simple sequence");
      Register_Routine
        (T, Test_Sequence_Of_Mappings'Access, "Sequence of mappings");
      Register_Routine
        (T, Test_Mapping_With_Sequence'Access, "Mapping containing sequence");
      Register_Routine
        (T, Test_Duplicate_Key_Rejected'Access, "Duplicate key rejected");
      Register_Routine (T, Test_Navigate_Path'Access, "Navigate with path");
      Register_Routine
        (T, Test_Get_String'Access, "Get_String convenience function");
      Register_Routine
        (T, Test_Parse_Document'Access,
         "Parse_Document with automatic memory management");
      Register_Routine (T, Test_Free_Node'Access, "Free_Node deallocates tree");
   end Register_Tests;

   procedure Test_Empty_Input (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse ("");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Scalar (Doc.all), "Empty input should be scalar");
      Assert (Scalar_Node (Doc.all).Value = "", "Should be empty string");
   end Test_Empty_Input;

   procedure Test_Simple_Scalar (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse ("hello world");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Scalar (Doc.all), "Should be a scalar");
      Assert
        (Scalar_Node (Doc.all).Value = "hello world",
         "Should be 'hello world'");
   end Test_Simple_Scalar;

   procedure Test_Simple_Mapping (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Doc : Node_Access;
      M   : Map_Node;
   begin
      Doc := Tinyaml.Parser.Parse ("key: value");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("key"), "Should contain 'key'");
      Assert
        (Scalar_Node (M.Get ("key").all).Value = "value",
         "Value should be 'value'");
   end Test_Simple_Mapping;

   procedure Test_Nested_Mapping (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 5432";
      Doc : Node_Access;
      M   : Map_Node;
      Db  : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("database"), "Should contain 'database'");
      Db := M.Get ("database");
      Assert (Is_Map (Db.all), "database should be a map");
      Assert
        (Scalar_Node (Map_Node (Db.all).Get ("host").all).Value = "localhost",
         "host should be 'localhost'");
      Assert
        (Scalar_Node (Map_Node (Db.all).Get ("port").all).Value = "5432",
         "port should be '5432'");
   end Test_Nested_Mapping;

   procedure Test_Simple_Sequence (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "- apple" & ASCII.LF &
        "- banana" & ASCII.LF &
        "- cherry";
      Doc : Node_Access;
      S   : Sequence_Node;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Sequence (Doc.all), "Should be a sequence");
      S := Sequence_Node (Doc.all);
      Assert (S.Length = 3, "Should have 3 items");
      Assert
        (Scalar_Node (S.Element (1).all).Value = "apple",
         "First should be 'apple'");
      Assert
        (Scalar_Node (S.Element (2).all).Value = "banana",
         "Second should be 'banana'");
      Assert
        (Scalar_Node (S.Element (3).all).Value = "cherry",
         "Third should be 'cherry'");
   end Test_Simple_Sequence;

   procedure Test_Sequence_Of_Mappings (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "- name: Alice" & ASCII.LF &
        "- name: Bob";
      Doc : Node_Access;
      S   : Sequence_Node;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Sequence (Doc.all), "Should be a sequence");
      S := Sequence_Node (Doc.all);
      Assert (S.Length = 2, "Should have 2 items");
      Assert (Is_Map (S.Element (1).all), "First item should be a map");
      Assert
        (Scalar_Node (Map_Node (S.Element (1).all).Get ("name").all).Value =
           "Alice",
         "First name should be 'Alice'");
   end Test_Sequence_Of_Mappings;

   procedure Test_Mapping_With_Sequence (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "fruits:" & ASCII.LF &
        "  - apple" & ASCII.LF &
        "  - banana";
      Doc : Node_Access;
      M   : Map_Node;
      S   : Sequence_Node;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("fruits"), "Should contain 'fruits'");
      Assert (Is_Sequence (M.Get ("fruits").all), "fruits should be sequence");
      S := Sequence_Node (M.Get ("fruits").all);
      Assert (S.Length = 2, "Should have 2 items");
      Assert
        (Scalar_Node (S.Element (1).all).Value = "apple",
         "First should be 'apple'");
   end Test_Mapping_With_Sequence;

   procedure Test_Duplicate_Key_Rejected (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "key: value1" & ASCII.LF &
        "key: value2";
   begin
      begin
         declare
            Doc : constant Node_Access := Tinyaml.Parser.Parse (Input);
            pragma Unreferenced (Doc);
         begin
            Assert (False, "Should have raised Parse_Error for duplicate key");
         end;
      exception
         when Tinyaml.Parse_Error =>
            null;
      end;
   end Test_Duplicate_Key_Rejected;

   procedure Test_Navigate_Path (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "database:" & ASCII.LF &
        "  connection:" & ASCII.LF &
        "    host: localhost";
      Doc  : Node_Access;
      Host : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Host := Navigate (Doc, "database.connection.host");
      Assert (Host /= null, "Should find host");
      Assert
        (Scalar_Node (Host.all).Value = "localhost",
         "host should be 'localhost'");
   end Test_Navigate_Path;

   procedure Test_Get_String (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "config:" & ASCII.LF &
        "  name: myapp";
      Doc : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Get_String (Doc, "config.name") = "myapp", "Should be 'myapp'");
   end Test_Get_String;

   procedure Test_Parse_Document (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 5432";
   begin
      declare
         Doc : constant Document := Tinyaml.Parser.Parse_Document (Input);
      begin
         Assert (not Is_Empty (Doc), "Document should not be empty");
         Assert (Root (Doc) /= null, "Root should not be null");
         Assert (Is_Map (Root (Doc).all), "Root should be a map");
         Assert
           (Get_String (Root (Doc), "database.host") = "localhost",
            "host should be 'localhost'");
         Assert
           (Get_String (Root (Doc), "database.port") = "5432",
            "port should be '5432'");
      end;
   end Test_Parse_Document;

   procedure Test_Free_Node (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Input : constant String :=
        "items:" & ASCII.LF &
        "  - one" & ASCII.LF &
        "  - two" & ASCII.LF &
        "  - three";
      Doc : Node_Access;
   begin
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Doc /= null, "Parse should return non-null");
      Assert (Is_Map (Doc.all), "Root should be a map");
      Free_Node (Doc);
      Assert (Doc = null, "After Free_Node, access should be null");
   end Test_Free_Node;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (new Test);
      return Result;
   end Suite;

end Tinyaml.Parser_Tests;
