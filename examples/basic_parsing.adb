--  Basic Parsing Example
--
--  Demonstrates parsing YAML without schema validation.
--  All values are strings until explicitly converted.

with Ada.Text_IO;
with Tinyaml.Parser;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;

procedure Basic_Parsing is
   use Ada.Text_IO;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Prelude;

   Config : constant String :=
     "database:" & ASCII.LF &
     "  host: localhost" & ASCII.LF &
     "  port: 5432" & ASCII.LF &
     "  name: myapp" & ASCII.LF &
     "features:" & ASCII.LF &
     "  - logging" & ASCII.LF &
     "  - metrics";

   Doc : Node_Access;
begin
   Put_Line ("Parsing configuration...");
   New_Line;

   Doc := Tinyaml.Parser.Parse (Config);

   --  Access nested values using path notation
   Put_Line ("Database host: " & Get_String (Doc, "database.host"));
   Put_Line ("Database port: " & Get_String (Doc, "database.port"));
   Put_Line ("Database name: " & Get_String (Doc, "database.name"));
   New_Line;

   --  Check for optional fields using Navigate (returns null if missing)
   declare
      Timeout : constant Node_Access := Navigate (Doc, "database.timeout");
   begin
      if Timeout /= null then
         Put_Line ("Database timeout: " & Value (Timeout));
      else
         Put_Line ("Database timeout: not specified (using default)");
      end if;
   end;
   New_Line;

   --  Alternative: use Contains on Map_Node
   declare
      Database : constant Node_Access := Navigate (Doc, "database");
   begin
      if Is_Map (Database.all) then
         if Map_Node (Database.all).Contains ("port") then
            Put_Line ("Port is configured: " &
              Value (Map_Node (Database.all).Get ("port")));
         end if;
         if not Map_Node (Database.all).Contains ("ssl") then
            Put_Line ("SSL not configured");
         end if;
      end if;
   end;
   New_Line;

   --  Access sequence items
   declare
      Features : constant Node_Access := Navigate (Doc, "features");
      Seq      : Sequence_Node;
   begin
      Put_Line ("Features:");
      if Features /= null and then Is_Sequence (Features.all) then
         Seq := Sequence_Node (Features.all);
         for I in 1 .. Seq.Length loop
            Put_Line ("  - " & Value (Seq.Element (I)));
         end loop;
      end if;
   end;
end Basic_Parsing;
