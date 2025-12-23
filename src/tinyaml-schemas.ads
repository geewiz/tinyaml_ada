--  TinyAML Schema Types - Define expected document structure
--

with Ada.Strings.Unbounded;

with Tinyaml.Nodes;

package Tinyaml.Schemas is

   ---------------------------------------------------------------------------
   --  Validation Info (detailed validation result)
   ---------------------------------------------------------------------------

   type Validation_Info is record
      Is_Valid : Boolean := True;
      Message  : Ada.Strings.Unbounded.Unbounded_String;
      Path     : Ada.Strings.Unbounded.Unbounded_String;
      Value    : Ada.Strings.Unbounded.Unbounded_String;  --  Offending value
   end record;

   ---------------------------------------------------------------------------
   --  Abstract Schema Base Type (public for derivation)
   ---------------------------------------------------------------------------

   type Schema is abstract tagged null record;

   function Is_Valid
     (S : Schema;
      N : Nodes.Node_Access) return Boolean is abstract;

   function Describe (S : Schema) return String is abstract;

   --  Detailed validation (not abstract - has default implementation)
   function Validate_Node
     (S : Schema;
      N : Nodes.Node_Access) return Validation_Info;

   ---------------------------------------------------------------------------
   --  Access type for polymorphic storage (used by child packages)
   ---------------------------------------------------------------------------

   type Schema_Access is access all Schema'Class;

end Tinyaml.Schemas;
