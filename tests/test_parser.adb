with Test_Harness;
with Tinyaml;
with Tinyaml.Documents;
with Tinyaml.Parser;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Map;
with Tinyaml.Nodes.Navigation;
with Tinyaml.Nodes.Scalar;
with Tinyaml.Nodes.Sequence;

package body Test_Parser is

   use Test_Harness;
   use Tinyaml.Documents;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Map;
   use Tinyaml.Nodes.Navigation;
   use Tinyaml.Nodes.Scalar;
   use Tinyaml.Nodes.Sequence;

   procedure Test_Empty_Input is
      Doc : Node_Access;
   begin
      Start_Test ("Empty input");
      Doc := Tinyaml.Parser.Parse ("");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Scalar (Doc.all), "Empty input should be scalar");
      Assert_Equal ("", Scalar_Node (Doc.all).Value);
      Pass;
   end Test_Empty_Input;

   procedure Test_Simple_Scalar is
      Doc : Node_Access;
   begin
      Start_Test ("Simple scalar document");
      Doc := Tinyaml.Parser.Parse ("hello world");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Scalar (Doc.all), "Should be a scalar");
      Assert_Equal ("hello world", Scalar_Node (Doc.all).Value);
      Pass;
   end Test_Simple_Scalar;

   procedure Test_Simple_Mapping is
      Doc : Node_Access;
      M   : Map_Node;
   begin
      Start_Test ("Simple mapping");
      Doc := Tinyaml.Parser.Parse ("key: value");
      Assert (Doc /= null, "Should return a node");
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("key"), "Should contain 'key'");
      Assert_Equal ("value", Scalar_Node (M.Get ("key").all).Value);
      Pass;
   end Test_Simple_Mapping;

   procedure Test_Nested_Mapping is
      Input : constant String :=
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 5432";
      Doc : Node_Access;
      M   : Map_Node;
      Db  : Node_Access;
   begin
      Start_Test ("Nested mapping");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("database"), "Should contain 'database'");
      Db := M.Get ("database");
      Assert (Is_Map (Db.all), "database should be a map");
      Assert_Equal ("localhost",
                    Scalar_Node (Map_Node (Db.all).Get ("host").all).Value);
      Assert_Equal ("5432",
                    Scalar_Node (Map_Node (Db.all).Get ("port").all).Value);
      Pass;
   end Test_Nested_Mapping;

   procedure Test_Simple_Sequence is
      Input : constant String :=
        "- apple" & ASCII.LF &
        "- banana" & ASCII.LF &
        "- cherry";
      Doc : Node_Access;
      S   : Sequence_Node;
   begin
      Start_Test ("Simple sequence");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Sequence (Doc.all), "Should be a sequence");
      S := Sequence_Node (Doc.all);
      Assert_Equal (3, S.Length, "Should have 3 items");
      Assert_Equal ("apple", Scalar_Node (S.Element (1).all).Value);
      Assert_Equal ("banana", Scalar_Node (S.Element (2).all).Value);
      Assert_Equal ("cherry", Scalar_Node (S.Element (3).all).Value);
      Pass;
   end Test_Simple_Sequence;

   procedure Test_Sequence_Of_Mappings is
      Input : constant String :=
        "- name: Alice" & ASCII.LF &
        "- name: Bob";
      Doc : Node_Access;
      S   : Sequence_Node;
   begin
      Start_Test ("Sequence of mappings");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Sequence (Doc.all), "Should be a sequence");
      S := Sequence_Node (Doc.all);
      Assert_Equal (2, S.Length, "Should have 2 items");
      Assert (Is_Map (S.Element (1).all), "First item should be a map");
      Assert_Equal ("Alice",
        Scalar_Node (Map_Node (S.Element (1).all).Get ("name").all).Value);
      Pass;
   end Test_Sequence_Of_Mappings;

   procedure Test_Mapping_With_Sequence is
      Input : constant String :=
        "fruits:" & ASCII.LF &
        "  - apple" & ASCII.LF &
        "  - banana";
      Doc : Node_Access;
      M   : Map_Node;
      S   : Sequence_Node;
   begin
      Start_Test ("Mapping containing sequence");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Is_Map (Doc.all), "Should be a map");
      M := Map_Node (Doc.all);
      Assert (M.Contains ("fruits"), "Should contain 'fruits'");
      Assert (Is_Sequence (M.Get ("fruits").all), "fruits should be sequence");
      S := Sequence_Node (M.Get ("fruits").all);
      Assert_Equal (2, S.Length, "Should have 2 items");
      Assert_Equal ("apple", Scalar_Node (S.Element (1).all).Value);
      Pass;
   end Test_Mapping_With_Sequence;

   procedure Test_Duplicate_Key_Rejected is
      Input : constant String :=
        "key: value1" & ASCII.LF &
        "key: value2";
   begin
      Start_Test ("Duplicate key rejected");
      begin
         declare
            Doc : constant Node_Access := Tinyaml.Parser.Parse (Input);
            pragma Unreferenced (Doc);
         begin
            Fail ("Should have raised Parse_Error for duplicate key");
         end;
      exception
         when Tinyaml.Parse_Error =>
            Pass;
      end;
   end Test_Duplicate_Key_Rejected;

   procedure Test_Navigate_Path is
      Input : constant String :=
        "database:" & ASCII.LF &
        "  connection:" & ASCII.LF &
        "    host: localhost";
      Doc  : Node_Access;
      Host : Node_Access;
   begin
      Start_Test ("Navigate with path");
      Doc := Tinyaml.Parser.Parse (Input);
      Host := Navigate (Doc, "database.connection.host");
      Assert (Host /= null, "Should find host");
      Assert_Equal ("localhost", Scalar_Node (Host.all).Value);
      Pass;
   end Test_Navigate_Path;

   procedure Test_Get_String is
      Input : constant String :=
        "config:" & ASCII.LF &
        "  name: myapp";
      Doc : Node_Access;
   begin
      Start_Test ("Get_String convenience function");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert_Equal ("myapp", Get_String (Doc, "config.name"));
      Pass;
   end Test_Get_String;

   procedure Test_Parse_Document is
      Input : constant String :=
        "database:" & ASCII.LF &
        "  host: localhost" & ASCII.LF &
        "  port: 5432";
   begin
      Start_Test ("Parse_Document with automatic memory management");
      declare
         Doc : constant Document := Tinyaml.Parser.Parse_Document (Input);
      begin
         Assert (not Is_Empty (Doc), "Document should not be empty");
         Assert (Root (Doc) /= null, "Root should not be null");
         Assert (Is_Map (Root (Doc).all), "Root should be a map");
         Assert_Equal ("localhost", Get_String (Root (Doc), "database.host"));
         Assert_Equal ("5432", Get_String (Root (Doc), "database.port"));
      end;
      --  Document goes out of scope here, memory is automatically freed
      Pass;
   end Test_Parse_Document;

   procedure Test_Free_Node is
      Input : constant String :=
        "items:" & ASCII.LF &
        "  - one" & ASCII.LF &
        "  - two" & ASCII.LF &
        "  - three";
      Doc : Node_Access;
   begin
      Start_Test ("Free_Node deallocates tree");
      Doc := Tinyaml.Parser.Parse (Input);
      Assert (Doc /= null, "Parse should return non-null");
      Assert (Is_Map (Doc.all), "Root should be a map");
      --  Manually free the tree
      Free_Node (Doc);
      Assert (Doc = null, "After Free_Node, access should be null");
      Pass;
   end Test_Free_Node;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Initialize ("Parser Tests");

      Test_Empty_Input;
      Test_Simple_Scalar;
      Test_Simple_Mapping;
      Test_Nested_Mapping;
      Test_Simple_Sequence;
      Test_Sequence_Of_Mappings;
      Test_Mapping_With_Sequence;
      Test_Duplicate_Key_Rejected;
      Test_Navigate_Path;
      Test_Get_String;
      Test_Parse_Document;
      Test_Free_Node;
   end Run_Tests;

end Test_Parser;
