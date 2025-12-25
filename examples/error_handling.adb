--  Error Handling Example
--
--  Demonstrates handling parse errors and validation failures.

with Ada.Text_IO;
with Ada.Exceptions;
with Tinyaml;
with Tinyaml.Parser;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;

procedure Error_Handling is
   use Ada.Text_IO;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Prelude;

   --  Invalid YAML: duplicate keys
   Bad_Yaml : constant String :=
     "name: Alice" & ASCII.LF &
     "name: Bob";

   --  Invalid YAML: flow style (not supported)
   Flow_Style : constant String := "{key: value}";

   Doc : Node_Access;
   pragma Unreferenced (Doc);
begin
   --  Example 1: Duplicate key error
   Put_Line ("Parsing YAML with duplicate keys...");
   begin
      Doc := Tinyaml.Parser.Parse (Bad_Yaml);
      Put_Line ("  Unexpected success!");
   exception
      when E : Tinyaml.Parse_Error =>
         Put_Line ("  Parse error: " & Ada.Exceptions.Exception_Message (E));
   end;
   New_Line;

   --  Example 2: Unsupported syntax
   Put_Line ("Parsing flow-style YAML (not supported)...");
   begin
      Doc := Tinyaml.Parser.Parse (Flow_Style);
      Put_Line ("  Unexpected success!");
   exception
      when E : Tinyaml.Parse_Error =>
         Put_Line ("  Parse error: " & Ada.Exceptions.Exception_Message (E));
   end;
   New_Line;

   --  Example 3: Safe navigation with null checks
   Put_Line ("Safe navigation with null checks...");
   declare
      Config : constant String := "database:" & ASCII.LF & "  host: localhost";
      Root   : Node_Access;
      Found  : Node_Access;
   begin
      Root := Tinyaml.Parser.Parse (Config);

      --  This path exists
      Found := Navigate (Root, "database.host");
      if Found /= null then
         Put_Line ("  Found: database.host = " & Value (Found));
      end if;

      --  This path doesn't exist
      Found := Navigate (Root, "database.port");
      if Found = null then
         Put_Line ("  Not found: database.port");
      end if;
   end;
end Error_Handling;
