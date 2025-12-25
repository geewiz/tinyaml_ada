--  Error Handling Example
--
--  Demonstrates handling parse errors and validation failures.
--  Shows both Document-based API and manual memory management patterns.

with Ada.Text_IO;
with Ada.Exceptions;
with Tinyaml;
with Tinyaml.Documents;
with Tinyaml.Parser;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;

procedure Error_Handling is
   use Ada.Text_IO;
   use Tinyaml.Documents;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Prelude;

   --  Invalid YAML: duplicate keys
   Bad_Yaml : constant String :=
     "name: Alice" & ASCII.LF &
     "name: Bob";

   --  Invalid YAML: flow style (not supported)
   Flow_Style : constant String := "{key: value}";
begin
   --  Example 1: Duplicate key error (exception handling with Document)
   Put_Line ("Parsing YAML with duplicate keys...");
   begin
      declare
         Doc : constant Document := Tinyaml.Parser.Parse_Document (Bad_Yaml);
         pragma Unreferenced (Doc);
      begin
         Put_Line ("  Unexpected success!");
      end;
   exception
      when E : Tinyaml.Parse_Error =>
         Put_Line ("  Parse error: " & Ada.Exceptions.Exception_Message (E));
   end;
   New_Line;

   --  Example 2: Unsupported syntax
   Put_Line ("Parsing flow-style YAML (not supported)...");
   begin
      declare
         Doc : constant Document := Tinyaml.Parser.Parse_Document (Flow_Style);
         pragma Unreferenced (Doc);
      begin
         Put_Line ("  Unexpected success!");
      end;
   exception
      when E : Tinyaml.Parse_Error =>
         Put_Line ("  Parse error: " & Ada.Exceptions.Exception_Message (E));
   end;
   New_Line;

   --  Example 3: Safe navigation with null checks
   --  Document automatically frees memory when it goes out of scope
   Put_Line ("Safe navigation with null checks...");
   declare
      Config : constant String := "database:" & ASCII.LF & "  host: localhost";
      Doc    : constant Document := Tinyaml.Parser.Parse_Document (Config);
      Found  : Node_Access;
   begin
      --  This path exists
      Found := Navigate (Root (Doc), "database.host");
      if Found /= null then
         Put_Line ("  Found: database.host = " & Value (Found));
      end if;

      --  This path doesn't exist
      Found := Navigate (Root (Doc), "database.port");
      if Found = null then
         Put_Line ("  Not found: database.port");
      end if;
   end;
   --  Doc is finalized here, freeing all nodes
end Error_Handling;
