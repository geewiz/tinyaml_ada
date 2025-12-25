--  Schema Validation Example
--
--  Demonstrates defining a schema and validating YAML against it.
--  Schemas ensure your configuration has the expected structure.

with Ada.Text_IO;
with Tinyaml.Nodes;
with Tinyaml.Nodes.Prelude;
with Tinyaml.Schemas.Prelude;
with Tinyaml.Validation;

procedure With_Validation is
   use Ada.Text_IO;
   use Tinyaml.Nodes;
   use Tinyaml.Nodes.Prelude;
   use Tinyaml.Schemas.Prelude;
   use Tinyaml.Validation;

   --  Build nested schema: database fields first, then add to app schema
   Db_Schema  : Map_Schema;
   App_Schema : Map_Schema;

   --  Valid configuration
   Valid_Config : constant String :=
     "name: myapp" & ASCII.LF &
     "database:" & ASCII.LF &
     "  host: localhost" & ASCII.LF &
     "  port: 5432";

   --  Invalid configuration (database.port out of range)
   Invalid_Config : constant String :=
     "name: myapp" & ASCII.LF &
     "database:" & ASCII.LF &
     "  host: localhost" & ASCII.LF &
     "  port: 99999";

   Result : Validation_Result;
   Db_Node : Node_Access;
begin
   --  Define nested database schema
   Db_Schema.Str ("host");
   Db_Schema.Int ("port", Constraint => (Min => 1, Max => 65535));

   --  Define app schema with nested database
   App_Schema.Str ("name");
   App_Schema.Map ("database", Db_Schema);

   --  Validate good config
   Put_Line ("Validating good configuration...");
   Result := Parse_And_Validate (Valid_Config, App_Schema);

   if Result.Is_Valid then
      Put_Line ("  Valid!");
      Put_Line ("  Name: " & Get_String (Result.Root, "name"));
      Db_Node := Navigate (Result.Root.all, "database");
      Put_Line ("  Database host: " & Get_String (Db_Node, "host"));
      Put_Line ("  Database port: " & Get_String (Db_Node, "port"));
   end if;
   New_Line;

   --  Validate bad config
   Put_Line ("Validating bad configuration (database.port out of range)...");
   Result := Parse_And_Validate (Invalid_Config, App_Schema);

   if not Result.Is_Valid then
      Put_Line ("  Invalid: " & Result.Error_Message);
      if Result.Error_Path /= "" then
         Put_Line ("  At path: " & Result.Error_Path);
      end if;
      if Result.Error_Value /= "" then
         Put_Line ("  Got value: " & Result.Error_Value);
      end if;
   end if;
end With_Validation;
